# HPC Model Training Repository

## Overview

This repository contains code for training machine learning models on an HPC system. The training process is optimized for multi-GPU usage via the `accelerate` library and is configured to run on 4 GPUs by default. The number of GPUs can be modified in the `config.yaml` file by adjusting the `num_processes` parameter.  
Codes are heaviliy borrowed from [here](https://github.com/huggingface/transformers/blob/main/examples/research_projects/codeparrot)

## Prerequisites

Before running the training jobs, ensure that:

- You have access to an HPC system with multiple GPUs.
- You have installed the necessary dependencies (see [Installation](#installation)).
- You are logged into your Hugging Face and Weights & Biases (W&B) accounts.

## Installation

To set up the environment, install the required dependencies:

```bash
pip install -r ./requirements.txt
```

Additionally, configure the `accelerate` library by running:

```bash
accelerate config
```

## Running Training Jobs

To start training, simply execute the following command:

```bash
bash initiator.sh
```

### Expected Files and Directories

- **`model_list.txt`**: A text file containing the list of model checkpoints to be trained.
- **Per-model Directories**: For each model in `model_list.txt`, a separate directory will be created, where logs and checkpoints will be stored.
- **`accerelate_job.sh`**: Modify the paths in this file to point to the correct train and validation datasets.

### Modifying the Number of GPUs

To adjust the number of GPUs used for training, edit the `config.yaml` file and modify the `num_processes` parameter:

```yaml
num_processes: <desired_number_of_gpus>
```

## Logging and Checkpoints

- All training logs and model checkpoints are stored in the respective directories created for each model.
- Ensure that you are authenticated with Hugging Face and Weights & Biases (Wandb) before running the training.

## Support

For any issues or questions, please open an issue in this repository or contact the maintainers.

## License

This project is licensed under the `MIT` License.
