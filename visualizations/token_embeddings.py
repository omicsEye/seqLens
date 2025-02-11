import pandas as pd
import numpy as np
from sklearn.preprocessing import StandardScaler
import umap.umap_ as umap

# Define the base path to token embedding files
path_list = "/Users/mbagh/Library/CloudStorage/Box-Box/seqSightModels/MLM/fine_tune/embeddings/tokenizer_embeddings"
# Suffixes corresponding to different model variants
suffix_list = ["", "-Ms", "-Mp", "-Me"]

embedding_list = []  # List to store UMAP-transformed dataframes
raw_embedding_list = []  # List to store raw embeddings
distance_list = []  # List to store distance vectors
cosine_similarity_list = []  # List to store cosine similarity matrices
# Process each file individually
for suffix in suffix_list:
    path = f"{path_list}{suffix}.csv"  # Construct file path
    tmp = pd.read_csv(path)  # Read CSV file into DataFrame
    raw_embedding_list.append(np.array(tmp.iloc[:, :768]))  # Store raw embeddings
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
    if suffix == "":
        reducer = umap.UMAP(n_neighbors=15, min_dist=0.1, n_components=2)
        embedding = reducer.fit_transform(scaled_embeddings)
    else:
        embedding = reducer.transform(scaled_embeddings)

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

# calculate row-wise distances between embeddings
for i in range(len(raw_embedding_list)):
    distances = np.linalg.norm(raw_embedding_list[i] - raw_embedding_list[0], axis=1)
    distance_list.append(distances)

# Concatenate all distance vectors column-wise
final_distances = np.column_stack(distance_list)
final_distances = pd.DataFrame(final_distances)
# drop the base model column
final_distances = final_distances.drop(columns=0)
final_distances.columns = ["Ms", "Mp", "Me"]
final_distances['token'] = tmp['token']  # Retrieve original index
final_distances.to_csv("token_distances.csv", index=False)  # Save distance data to a CSV file

def cosine_similarity(matrix_a, matrix_b):
    """
    Calculates the cosine similarity between rows of two matrices.

    Args:
        matrix_a (numpy.ndarray): The first matrix.
        matrix_b (numpy.ndarray): The second matrix.

    Returns:
        numpy.ndarray: A matrix containing the cosine similarity between each row of matrix_a and each row of matrix_b.
    """
    
    if matrix_a.shape[1] != matrix_b.shape[1]:
        raise ValueError("Matrices must have the same number of columns")
    
    a_norm = np.linalg.norm(matrix_a, axis=1, keepdims=True)
    b_norm = np.linalg.norm(matrix_b, axis=1, keepdims=True)
    
    similarity_matrix = np.dot(matrix_a, matrix_b.T) / np.dot(a_norm, b_norm.T)
    return np.diag(similarity_matrix)

# Calculate cosine similarity between embeddings
for i in range(len(raw_embedding_list)):
    if i == 0:
        continue
    similarity = cosine_similarity(raw_embedding_list[i], raw_embedding_list[0])
    cosine_similarity_list.append(similarity)

# Concatenate all cosine similarity matrices column-wise
final_cosine_similarity = np.column_stack(cosine_similarity_list)
final_cosine_similarity = pd.DataFrame(final_cosine_similarity)
final_cosine_similarity.columns = ["Ms", "Mp", "Me"]
final_cosine_similarity['token'] = tmp['token']  # Retrieve original index
final_cosine_similarity.to_csv("token_cosine_similarity.csv", index=False)  # Save cosine similarity data
