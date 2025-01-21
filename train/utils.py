import logging
from pathlib import Path
from accelerate import Accelerator
import datasets
import transformers
from datasets import load_dataset
from torch.utils.data import DataLoader
from transformers import DataCollatorForLanguageModeling
from accelerate.utils import ProjectConfiguration
from pynvml import *


def print_gpu_utilization():
    nvmlInit()
    handle = nvmlDeviceGetHandleByIndex(0)
    info = nvmlDeviceGetMemoryInfo(handle)
    print(f"GPU memory occupied: {info.used//1024**2} MB.")


def print_summary(result):
    print(f"Time: {result.metrics['train_runtime']:.2f}")
    print(f"Samples/second: {result.metrics['train_samples_per_second']:.2f}")
    print_gpu_utilization()




def get_grouped_params(model, args, no_decay=["bias", "ln_1.weight", "ln_2.weight", "ln_f.weight"]):
    params_with_wd, params_without_wd = [], []
    for n, p in model.named_parameters():
        if any(nd in n for nd in no_decay):
            params_without_wd.append(p)
        else:
            params_with_wd.append(p)
    return [
        {"params": params_with_wd, "weight_decay": args.weight_decay},
        {"params": params_without_wd, "weight_decay": 0.0},
    ]


def setup_logging(args, accelerator: Accelerator):
    project_name = args.model_ckpt.split("/")[-1]
    logger = logging.getLogger(__name__)
    log_dir = Path(args.save_dir) / "log/"
    log_dir.mkdir(exist_ok=True, parents=True)
    filename = f"debug_{accelerator.process_index}.log"
    logging.basicConfig(
        format="%(asctime)s - %(levelname)s - %(name)s - %(message)s",
        datefmt="%m/%d/%Y %H:%M:%S",
        level=logging.INFO,
        handlers=[logging.FileHandler(log_dir / filename), logging.StreamHandler()],
    )
    if accelerator.is_main_process:  # we only want to setup logging once
        accelerator.init_trackers(project_name, vars(args))
        run_name = accelerator.trackers[0].run.name
        logger.setLevel(logging.INFO)
        datasets.utils.logging.set_verbosity_info()
        transformers.utils.logging.set_verbosity_info()
    else:
        run_name = ""
        logger.setLevel(logging.ERROR)
        datasets.utils.logging.set_verbosity_error()
        transformers.utils.logging.set_verbosity_error()
    return logger, run_name


def create_dataloaders(args, tokenizer):
    ds_kwargs = {"streaming": True}

    def encode(examples):
        return tokenizer(examples['sequence'], truncation=True, padding='max_length', max_length=args.max_length)

    # load the train dataset
    data_files={"train": args.train_file, "valid": args.valid_file}
    train_dataset = load_dataset('csv', data_files=data_files,
                                 split='train',
                                 delimiter='\t',
                                 streaming=True)
    # load the validation dataset
    valid_dataset = load_dataset('csv', data_files=data_files,
                                 split='valid',
                                 delimiter='\t',
                                 streaming=True)
    # valid_dataset = clean_columns(valid_dataset)

    cols_to_remove = ['split', 'assembly_id', 'assembly_accession',
                      'start', 'sequence_length', 'seq_id', 'sequence']
    train_dataset = train_dataset.map(encode, batched=True, remove_columns=cols_to_remove)
    valid_dataset = valid_dataset.map(encode, batched=True, remove_columns=cols_to_remove)

    # shuffle and batch the datasets
    train_dataset = train_dataset.shuffle(seed=42, buffer_size=args.shuffle_buffer)
    train_dataloader = DataLoader(train_dataset,
                                  collate_fn=DataCollatorForLanguageModeling(tokenizer,
                                                                             mlm=True,
                                                                             mlm_probability=args.mlm_probability),
                                  batch_size=args.train_batch_size)
    eval_dataloader = DataLoader(valid_dataset,
                                 collate_fn=DataCollatorForLanguageModeling(tokenizer,
                                                                            mlm=True,
                                                                            mlm_probability=args.mlm_probability),
                                 batch_size=args.valid_batch_size)
    return train_dataloader, eval_dataloader


def log_metrics(step, metrics, accelerator=None, logger=None):
    logger.info(f"Step {step}: {metrics}")
    if accelerator.is_main_process:
        accelerator.log(metrics, step)

def compute_tflops(elapsed_time, accelerator, args, model, tokenizer):
    # TFLOPs formula (from Equation 3 in Section 5.1 of https://arxiv.org/pdf/2104.04473.pdf).
    config_model = accelerator.unwrap_model(model).config
    checkpoint_factor = 4 if args.gradient_checkpointing else 3
    batch_size = args.train_batch_size * accelerator.state.num_processes * args.gradient_accumulation_steps
    if hasattr(config_model, "n_layer"):
        n_layer = config_model.n_layer
    elif hasattr(config_model, "num_hidden_layers"):
        n_layer = config_model.num_hidden_layers
    else:
        raise ValueError("Model config doesn't have 'n_layer' or 'num_hidden_layers' attribute")

    if hasattr(config_model, "n_embd"):
        n_embd = config_model.n_embd
    elif hasattr(config_model, "max_position_embeddings"):
        n_embd = config_model.max_position_embeddings
    else:
        raise ValueError("Model config doesn't have 'n_embd' or 'max_position_embeddings' attribute")

    factor = 24 * checkpoint_factor * batch_size * args.max_length * n_layer * (n_embd**2)
    flops_per_iteration = factor * (
        1.0
        + (args.max_length / (6.0 * n_embd))
        + (tokenizer.vocab_size / (16.0 * n_layer * n_embd))
    )
    tflops = flops_per_iteration / (elapsed_time * accelerator.state.num_processes * (10**12))
    return tflops


