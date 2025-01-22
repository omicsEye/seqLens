#!/bin/bash
#SBATCH --partition=large-gpu
#SBATCH --time=1-00:00:00
#SBATCH --nodes=1
# #SBATCH --job-name=$1
# #SBATCH -o "slurm_logs/$1_$2.out"
# #SBATCH -e "slurm_logs/$1_$2.err"

# Load necessary modules
module load cuda/12.2
module load cudnn/8.1.1

random_seed=123

python nt_cv_local.py --model_id "$1" \
        --dataset_name "$2" \
	--random_seed "$random_seed"
