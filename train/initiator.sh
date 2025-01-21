#!/bin/bash
# This script is used to initiate the process of the model training

input_list="model_list.txt"
# for each model in the model_list.txt file initiate the training process
while read model_name
do
    echo "Initiating training for model: $model_name"
    model_dir=${model_name}
    mkdir -p $model_dir
    check if $model_name/$model_name exists
    if [ -d "$model_name/$model_name" ]; then
        echo "Model $model_name already exists. Skipping training for this model."
        continue
    fi
    cp *.py $model_dir
    cp *.yaml $model_dir
    cp accelerate_job.sh $model_dir
    cd $model_dir
    sbatch  --job-name=$model_name accelerate_job.sh $model_name
    cd ..
    echo "Training initiated for model: $model_name"
done < $input_list