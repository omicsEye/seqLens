#!/bin/bash
#SBATCH --partition=large-gpu
#SBATCH --nodes=1
#SBATCH --time=7-00:00:00
#SBATCH -o out.out
#SBATCH -e err.err

# Load necessary modules
module load cuda/12.2
module load cudnn/8.1.1

# modify the paths to the data files!
train_file=/PATH/TO/THE/TRAIN/FILE
valid_file=/PATH/TO/THE/VALIDATION/FILE


accelerate launch --config_file config_file.yaml train.py --model_ckpt $1 \
                        --save_dir $1 \
                        --train_batch_size 16 \
                        --valid_batch_size 64 \
                        --learning_rate 5e-4 \
                        --gradient_accumulation_steps 16 \
                        --max_train_steps 150000 \
                        --save_checkpoint_steps 5000 \
                        --from_scratch True \
                        --train_file $train_file \
                        --valid_file $valid_file \

