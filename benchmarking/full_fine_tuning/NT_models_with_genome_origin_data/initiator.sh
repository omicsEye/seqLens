#!/bin/bash

# Define paths to files containing model names and data names
model_names_file="model_list_all_benchmark.txt"
data_names_file="data_names.txt"

# Function to check if directory exists
dir_exists() {
    if [ -d "$1" ]; then
        return 0  # Directory exists
    else
        return 1  # Directory does not exist
    fi
}

# Loop over each line in the model names file
while IFS= read -r model_name; do
    # Loop over each line in the data names file
    while IFS= read -r data_name; do
        file_name=results/"$model_name"_results_"$data_name".csv
        # check if the file already exists
        if [ -e "$file_name" ]; then
            # echo "File $file_name already exists"
            continue
        else
            # Call the batch file script with model_name and data_name as arguments
            job_name="$model_name"_"$data_name"
            echo $job_name
            sbatch -J "$job_name" -o slurm_logs/$job_name.out -e slurm_logs/$job_name.err job_submission.sh "$model_name" "$data_name"
        fi
        
    done < "$data_names_file"
done < "$model_names_file"
