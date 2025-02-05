import argparse
from transformers import (
    AutoModelForSequenceClassification,
    AutoTokenizer,
    TrainingArguments,
    Trainer,
    EarlyStoppingCallback,
)
import torch
from sklearn.metrics import matthews_corrcoef, f1_score, accuracy_score
from datasets import load_dataset, Dataset
from sklearn.model_selection import KFold
import pandas as pd
import numpy as np
import random
import os

# Argument parser
parser = argparse.ArgumentParser(
    description="Fine-tune DNA encoder model with 10-fold cross-validation"
)
parser.add_argument(
    "--model_id", type=str, help="ID of the pre-trained model", required=True
)
parser.add_argument(
    "--dataset_name",
    type=str,
    help="Name of the dataset for fine-tuning",
    required=True,
)
parser.add_argument(
    "--random_seed", type=int, help="Random seed for reproducibility", required=True
)
args = parser.parse_args()

# Set the random seed
random_seed = args.random_seed
random.seed(random_seed)
np.random.seed(random_seed)
torch.manual_seed(random_seed)
if torch.cuda.is_available():
    torch.cuda.manual_seed_all(random_seed)

# Get model and dataset names from arguments
model_id = args.model_id
model_name = model_id.split("/")[-1]
dataset_name = args.dataset_name

model_ckpt = f"omicseye/{model_id}"
# model_ckpt = f"InstaDeepAI/{model_id}"
if model_ckpt.endswith("-at-base-nt"):
    tokenizer_source = "omicseye/seqLens_4096_512_89M-at-base"
else:
    tokenizer_source = model_ckpt
# Load the tokenizer
tokenizer = AutoTokenizer.from_pretrained(tokenizer_source)

# Load dataset
train_dataset = load_dataset(
    "InstaDeepAI/nucleotide_transformer_downstream_tasks_revised",
    dataset_name,
    split="train",
    streaming=False,
)

test_dataset = load_dataset(
    "InstaDeepAI/nucleotide_transformer_downstream_tasks_revised",
    dataset_name,
    split="test",
    streaming=False,
)

# get number of possible labels
num_labels = len(set(train_dataset["label"]))

# Extract sequences and labels
train_sequences = train_dataset["sequence"]
train_labels = train_dataset["label"]

test_sequences = test_dataset["sequence"]
test_labels = test_dataset["label"]

# Convert to Dataset format
ds_test = Dataset.from_dict({"data": test_sequences, "labels": test_labels})


# Tokenization function
def tokenize_function(examples):
    outputs = tokenizer(examples["data"])
    return {
        "input_ids": outputs["input_ids"],
        "attention_mask": outputs["attention_mask"],
    }


# Tokenize test data
tokenized_datasets_test = ds_test.map(
    tokenize_function,
    batched=True,
    remove_columns=["data"],
)

# Cross-validation and training configuration
kf = KFold(n_splits=10, shuffle=True, random_state=random_seed)
batch_size = 16
results = []


# Training arguments template
def get_training_args(fold):
    return TrainingArguments(
        f"checkpoints/{model_name}-finetuned-{dataset_name}-fold{fold}",
        evaluation_strategy="steps",
        save_strategy="steps",
        learning_rate=1e-5,
        per_device_train_batch_size=batch_size,
        gradient_accumulation_steps=1,
        per_device_eval_batch_size=64,
        num_train_epochs=2,
        logging_steps=500,
        load_best_model_at_end=True,
        metric_for_best_model="mcc",
        label_names=["labels"],
        dataloader_drop_last=True,
        max_steps=10000,
        report_to="none",  # Disable logging to Hugging Face
    )


# Metrics computation function
def compute_metrics(eval_pred):
    predictions = np.argmax(
        eval_pred.predictions, axis=-1
    )
    references = eval_pred.label_ids
    return {
        "accuracy": accuracy_score(references, predictions),
        "f1_score": f1_score(references, predictions, average="weighted"),
        "mcc": matthews_corrcoef(references, predictions),
    }


device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
best_mcc = 0  # Track best F1 score across all folds
best_model = None  # Track best model

# Perform 10-fold cross-validation
for fold, (train_idx, val_idx) in enumerate(kf.split(train_sequences)):
    print(f"Fold {fold+1}")

    # Load model
    model = AutoModelForSequenceClassification.from_pretrained(
        model_ckpt, num_labels=num_labels, trust_remote_code=True
    )

    # Prepare datasets for this fold
    train_fold_sequences = [train_sequences[i] for i in train_idx]
    val_fold_sequences = [train_sequences[i] for i in val_idx]
    train_fold_labels = [train_labels[i] for i in train_idx]
    val_fold_labels = [train_labels[i] for i in val_idx]

    ds_train = Dataset.from_dict(
        {"data": train_fold_sequences, "labels": train_fold_labels}
    )
    ds_val = Dataset.from_dict({"data": val_fold_sequences, "labels": val_fold_labels})

    # Tokenize datasets
    tokenized_datasets_train = ds_train.map(
        tokenize_function, batched=True, remove_columns=["data"]
    )
    tokenized_datasets_val = ds_val.map(
        tokenize_function, batched=True, remove_columns=["data"]
    )

    # Create training arguments for this fold
    args = get_training_args(fold)

    # EarlyStoppingCallback setup
    early_stopping_callback = EarlyStoppingCallback(
        early_stopping_patience=5,  # Number of evaluations with no improvement after which training will be stopped
        early_stopping_threshold=0.01,  # Minimum change to qualify as an improvement
    )
    # Initialize Trainer
    trainer = Trainer(
        model.to(device),
        args,
        train_dataset=tokenized_datasets_train,
        eval_dataset=tokenized_datasets_val,
        tokenizer=tokenizer,
        compute_metrics=compute_metrics,
        callbacks=[early_stopping_callback],
    )

    # Train and evaluate
    train_results = trainer.train()

    # Evaluate on validation set
    eval_results = trainer.evaluate()
    mcc_val = eval_results["eval_mcc"]
    if mcc_val > best_mcc:
        best_mcc = mcc_val
        best_model = model

    # Save metrics for validation set
    for metric, score in eval_results.items():
        results.append(
            {
                "model": model_name,
                "dataset": dataset_name,
                "evaluation_set": "validation",
                "metric": metric,
                "score": score,
            }
        )

# Evaluate the best model on the test set
best_trainer = Trainer(
    best_model.to(device),
    args,
    eval_dataset=tokenized_datasets_test,
    tokenizer=tokenizer,
    compute_metrics=compute_metrics,
)

test_results = best_trainer.evaluate()

# Save metrics for test set
for metric, score in test_results.items():
    results.append(
        {
            "model": model_name,
            "dataset": dataset_name,
            "evaluation_set": "test",
            "metric": metric,
            "score": score,
        }
    )

# Create 'results' folder if it doesn't exist
output_dir = "results"
os.makedirs(output_dir, exist_ok=True)

# Save results to a CSV file in the 'results' folder
output_file = os.path.join(output_dir, f"{model_name}_results_{dataset_name}.csv")
results_df = pd.DataFrame(results)
results_df.to_csv(output_file, index=False)
