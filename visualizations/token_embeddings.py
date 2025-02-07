import pandas as pd
import numpy as np
from sklearn.preprocessing import StandardScaler
import umap.umap_ as umap

# Define the base path to token embedding files
path_list = "/Users/mbagh/Library/CloudStorage/Box-Box/seqSightModels/MLM/fine_tune/embeddings/tokenizer_embeddings"
# Suffixes corresponding to different model variants
suffix_list = ["", "-Ms", "-Mp", "-Me"]

embedding_list = []  # List to store UMAP-transformed dataframes

# Process each file individually
for suffix in suffix_list:
    path = f"{path_list}{suffix}.csv"  # Construct file path
    tmp = pd.read_csv(path)  # Read CSV file into DataFrame

    # Assign a model label for differentiation
    tmp["model"] = "base" if suffix == "" else suffix[1:]

    # Filter tokens based on length constraints (only keep tokens of length 6 to 8)
    tmp = tmp[(tmp['token_length'] > 5) & (tmp['token_length'] < 9)]
    
    # Drop the token length column as it is no longer needed
    tmp.drop(columns=["token_length"], inplace=True)

    # Create a unique identifier combining token and model name
    tmp['token_model'] = tmp['token'] + "_" + tmp['model']
    
    # Drop original 'token' and 'model' columns as they are now encoded in 'token_model'
    tmp.drop(columns=["token", "model"], inplace=True)

    # Set 'token_model' as the index
    tmp.index = tmp['token_model']
    
    # Drop 'token_model' column after setting it as the index
    tmp.drop(columns=["token_model"], inplace=True)

    # Normalize embeddings using StandardScaler
    scaler = StandardScaler()
    scaled_embeddings = scaler.fit_transform(tmp)

    # Apply UMAP dimensionality reduction to each file individually
    reducer = umap.UMAP()
    embedding = reducer.fit_transform(scaled_embeddings)

    # Convert reduced embeddings into a DataFrame
    embedding_df = pd.DataFrame(embedding, columns=["x", "y"])
    embedding_df.index = tmp.index  # Retain original index

    # Reset index to retrieve 'token_model' column
    embedding_df.reset_index(inplace=True)

    # Split 'token_model' back into 'token' and 'model' components
    embedding_df[['token', 'model']] = embedding_df['token_model'].str.split('_', expand=True)
    
    # Drop the 'token_model' column as it has been decomposed
    embedding_df.drop(columns=["token_model"], inplace=True)

    # Append processed UMAP-transformed DataFrame to the list
    embedding_list.append(embedding_df)

# Concatenate all UMAP-transformed dataframes row-wise
final_embedding = pd.concat(embedding_list, ignore_index=True)

# Save the final processed embedding data to a CSV file
final_embedding.to_csv("token_embeddings.csv", index=False)
