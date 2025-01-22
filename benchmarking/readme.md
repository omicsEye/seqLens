# Benchmarking

This directory contains the benchmarking code for the project. It includes scripts for both full fine-tuning and LoRA.
The `combined_output.csv` file contains the results of the benchmarking process.

Each directory contains the following files:

- `initiator.sh`: The main script to start the benchmarking process.

- `A Python script`: Contains the code for benchmarking.

- `job_submission.sh`: Script for submitting jobs to the cluster.

- `data_names.txt`: Lists the datasets used for benchmarking.

- `model_list_all_benchmark.txt`: Lists the models to be benchmarked.

To run the benchmarking code, execute:

```bash
bash initiator.sh
```
