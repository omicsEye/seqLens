import json
from transformers import AutoTokenizer, AutoConfig
import torch
from accelerate import Accelerator, DistributedType
from transformers import AutoModelForMaskedLM, set_seed, HfArgumentParser, get_scheduler
from utils import get_grouped_params, setup_logging, log_metrics
from utils import compute_tflops, create_dataloaders, print_gpu_utilization
from arguments import TrainingArguments
from torch.optim import AdamW
import os
import time
from argparse import Namespace
from accelerate.utils import ProjectConfiguration

# Settings
parser = HfArgumentParser(TrainingArguments)
args = parser.parse_args()

# check if the shuffle buffer is divisible by the train batch size
if (
    args.shuffle_buffer % (args.train_batch_size * args.gradient_accumulation_steps)
    != 0
):
    args.shuffle_buffer = args.train_batch_size * args.gradient_accumulation_steps * 5
print(args)
# load model and tokenizer
tokenizer = AutoTokenizer.from_pretrained(args.model_ckpt)
if args.from_scratch:
    print("Training from scratch")
    config = AutoConfig.from_pretrained(args.model_ckpt)
    config.hidden_dropout_prob = 0
    config.attention_probs_dropout_prob = 0
    model = AutoModelForMaskedLM.from_config(config)
else:
    model = AutoModelForMaskedLM.from_pretrained(args.model_ckpt)
model_config = model.config.to_dict()

# print the model and tokenizer
print(tokenizer.vocab_size)
print(model)
print("initial GPU memory occupied:")
print_gpu_utilization()

# Enable gradient checkpointing
if args.gradient_checkpointing:
    model.gradient_checkpointing_enable()

# Load dataset and dataloader
train_dataloader, eval_dataloader = create_dataloaders(args=args, tokenizer=tokenizer)

# Initialize the accelerator
config = ProjectConfiguration(project_dir=args.save_dir, logging_dir="log")
accelerator = Accelerator(log_with=["wandb", "tensorboard"], project_config=config)

# Add accelerator state to the arguments
acc_state = {str(k): str(v) for k, v in accelerator.state.__dict__.items()}
args = Namespace(**vars(args), **acc_state)
samples_per_step = accelerator.state.num_processes * args.train_batch_size
print(args)

set_seed(args.seed)

# Logging
logger, run_name = setup_logging(args, accelerator=accelerator)
logger.info(accelerator.state)

# Prepare the optimizer and learning rate scheduler
optimizer = AdamW(get_grouped_params(model, args), lr=args.learning_rate)
lr_scheduler = get_scheduler(
    name=args.lr_scheduler_type,
    optimizer=optimizer,
    # 5% of the training steps
    num_warmup_steps=int(args.max_train_steps * 0.05),
    num_training_steps=args.max_train_steps * accelerator.state.num_processes,
)

# Register the lr_scheduler to the accelerator
accelerator.register_for_checkpointing(lr_scheduler)

# Prepare the model, optimizer, and dataloaders
model, optimizer, train_dataloader, eval_dataloader, lr_scheduler = accelerator.prepare(
    model, optimizer, train_dataloader, eval_dataloader, lr_scheduler
)
print("After accelerate init")
print_gpu_utilization()

# load in the weights and states from a previous save
if args.resume_from_checkpoint:
    if args.resume_from_checkpoint is not None or args.resume_from_checkpoint != "":
        accelerator.print(
            f"Resumed from checkpoint: {
                args.resume_from_checkpoint}"
        )
        accelerator.load_state(args.resume_from_checkpoint)
        path = os.path.basename(args.resume_from_checkpoint)
    else:
        # Get the most recent checkpoint
        dirs = [
            f.name for f in os.scandir(args.save_dir) if f.is_dir() and "step" in str(f)
        ]
        dirs.sort(key=os.path.getctime)
        # Sorts folders by date modified, most recent checkpoint is the last
        path = dirs[-1]
    # Extract the step of the checkpoint to continue from there
    training_difference = os.path.splitext(path)[0]
    resume_step = int(training_difference.replace("step_", ""))


# Evaluate the model
def evaluate(args):
    model.eval()
    losses = []
    for step, batch in enumerate(eval_dataloader):
        with torch.no_grad():
            outputs = model(**batch)
        loss = outputs.loss.repeat(args.valid_batch_size)
        losses.append(accelerator.gather(loss))
        if 0 < args.max_eval_steps <= step:
            break
    losses = torch.cat(losses)
    loss = losses.mean(dim=-1)
    try:
        perplexity = torch.exp(loss)
    except OverflowError:
        perplexity = float("inf")
    return loss.item(), perplexity.item()


def get_lr():
    return optimizer.param_groups[0]["lr"]


# Train model
model.train()
completed_steps = 0
small_step = 1
t_start = time.time()
loss_tracking = 0
epoch = 0

t1 = time.time()

while True:  # Use a while loop to control the maximum number of steps
    for step, batch in enumerate(train_dataloader, start=0):
        if args.resume_from_checkpoint and step < resume_step:
            continue  # we need to skip steps until we reach the resumed step

        loss = model(**batch).loss  # forward pass
        # gather the loss from all processes
        avg_loss = accelerator.gather(loss.repeat(args.train_batch_size)).mean()
        loss_tracking += (
            avg_loss.item() / args.gradient_accumulation_steps
        )  # accumulate the loss
        metrics = {
            "epoch": epoch,
            "small_steps": small_step,
            "epoch_step": step,
            "samples": small_step * samples_per_step,
            "loss_per_step/train": loss.item(),
        }
        log_metrics(
            step=completed_steps,
            metrics=metrics,
            accelerator=accelerator,
            logger=logger,
        )
        loss = loss / args.gradient_accumulation_steps  # scale the loss

        if small_step % args.gradient_accumulation_steps != 0:
            # Prevent backward from doing gradient all_reduce in every step
            if accelerator.distributed_type == DistributedType.MULTI_GPU:
                with model.no_sync():
                    accelerator.backward(loss)
            else:
                accelerator.backward(loss)
        else:
            # Prevent backward from doing gradient all_reduce in every step
            if accelerator.distributed_type == DistributedType.MULTI_GPU:
                with model.no_sync():
                    accelerator.backward(loss)
            else:
                accelerator.backward(loss)
            lr = get_lr()
            accelerator.clip_grad_norm_(model.parameters(), 1.0)
            optimizer.step()
            lr_scheduler.step()
            optimizer.zero_grad()
            elapsed_time = time.time() - t_start
            tflops = compute_tflops(
                elapsed_time=elapsed_time,
                accelerator=accelerator,
                args=args,
                model=model,
                tokenizer=tokenizer,
            )
            metrics = {
                "epoch": epoch,
                "completed_steps": completed_steps,
                "epoch_step": step,
                "samples": small_step * samples_per_step,
                "small_step": small_step,
                "loss/train": loss_tracking,
                "lr": lr,
                "tflops": tflops,
                "time_per_iteration": elapsed_time,
            }
            log_metrics(
                step=completed_steps,
                metrics=metrics,
                accelerator=accelerator,
                logger=logger,
            )
            t_start = time.time()
            loss_tracking = 0
            completed_steps += 1

            if completed_steps % args.save_checkpoint_steps == 0:
                logger.info("Evaluating and saving model checkpoint")
                # evaluate the model
                eval_loss, perplexity = evaluate(args)
                metrics = {
                    "epoch": epoch,
                    "completed_steps": completed_steps,
                    "epoch_step": step,
                    "samples": small_step * samples_per_step,
                    "small_step": small_step,
                    "loss/eval": eval_loss,
                    "perplexity": perplexity,
                }
                log_metrics(
                    step=completed_steps,
                    metrics=metrics,
                    accelerator=accelerator,
                    logger=logger,
                )
                accelerator.wait_for_everyone()
                save_dir = os.path.join(args.save_dir, f"step_{completed_steps}")
                accelerator.save_state(save_dir)
                # Save model configuration
                config_path = os.path.join(save_dir, "config.json")
                with open(config_path, "w") as config_file:
                    json.dump(model_config, config_file)
                # if accelerator.is_main_process:
                #     hf_repo.push_to_hub(commit_message=f"step {step}")
                model.train()
        print(
            "small step:",
            small_step,
            "completed steps:",
            completed_steps,
            "epoch:",
            epoch,
            "step:",
            step,
            "max steps:",
            args.max_train_steps,
            "samples:",
            samples_per_step * small_step,
        )
        if (
            completed_steps >= args.max_train_steps
        ):  # Check if we've reached the maximum number of steps
            break  # Exit the loop if maximum steps are reached
        small_step += 1
    if (
        completed_steps >= args.max_train_steps
    ):  # Check if we've reached the maximum number of steps
        break  # Exit the loop if maximum steps are reached
    epoch += 1

print("Time taken to train:", time.time() - t1)

print("After training")
print_gpu_utilization()
# Evaluate and save the last checkpoint
logger.info("Evaluating and saving model after training")
eval_loss, perplexity = evaluate(args)

metrics = {
    "epoch": epoch,
    "completed_steps": completed_steps - 1,
    "epoch_step": step,
    "samples": small_step * samples_per_step,
    "small_step": small_step,
    "loss/eval": eval_loss,
    "perplexity": perplexity,
}

log_metrics(
    step=completed_steps - 1, metrics=metrics, accelerator=accelerator, logger=logger
)
accelerator.wait_for_everyone()
unwrapped_model = accelerator.unwrap_model(model)
unwrapped_model.save_pretrained(args.save_dir, save_function=accelerator.save)
save_dir = os.path.join(args.save_dir, f"step_{completed_steps}")
accelerator.save_state(save_dir)
