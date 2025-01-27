# Classification Heads

This repository explores different classification heads that can be used with various pooling approaches in deep learning models.

## Pooling Approaches

### 1. Mean Pooling

Mean pooling computes the average of all token embeddings to generate a fixed-size representation.

```python
class EsmClassificationHead(nn.Module):
    """Head for sentence-level classification tasks, using mean-pooling."""

    def __init__(self, config):
        super().__init__()
        self.dense = nn.Linear(config.hidden_size, config.hidden_size)
        self.dropout = nn.Dropout(config.hidden_dropout_prob)
        self.out_proj = nn.Linear(config.hidden_size, config.num_labels)

    def forward(self, features, attention_mask, **kwargs):
        # Expand attention_mask for broadcasting to match features' last dimension
        expanded_mask = attention_mask.unsqueeze(-1).expand_as(features)
        
        # Masked features - set padding tokens to 0
        masked_features = features * expanded_mask
        
        # Sum and count valid (non-padding) tokens to get the average
        sum_features = masked_features.sum(dim=1)
        non_padding_count = expanded_mask.sum(dim=1).clamp(min=1)  # Avoid division by zero
        avg_features = sum_features / non_padding_count  # Average only non-padding tokens
        
        # Pass through dropout, dense layer, activation, another dropout, and output projection
        x = self.dense(avg_features)
        x = torch.tanh(x)
        x = self.dropout(x)
        x = self.out_proj(x)  # Final projection to output logits
        return x

```

### 2. Max Pooling

Max pooling selects the maximum value across token embeddings to form the final representation.

```python
class EsmClassificationHead(nn.Module):
    """Head for sentence-level classification tasks, using max-pooling."""

    def __init__(self, config):
        super().__init__()
        self.dense = nn.Linear(config.hidden_size, config.hidden_size)
        # self.conv1d = nn.Conv1d(in_channels=3, out_channels=1, kernel_size=1)  # Convolution over 3 channels
        self.dropout = nn.Dropout(config.hidden_dropout_prob)
        self.out_proj = nn.Linear(config.hidden_size, config.num_labels)

    def forward(self, features, attention_mask, **kwargs):
        # Expand attention_mask for broadcasting to match features' last dimension
        expanded_mask = attention_mask.unsqueeze(-1).expand_as(features)
        
        # Masked features - set padding tokens to 0
        masked_features = features * expanded_mask
    
        # Extract the max value of non-padding tokens
        max_features = masked_features.max(dim=1).values
        x = self.dense(max_features)
        # x = self.dense(x)
        x = torch.tanh(x)
        x = self.dropout(x)
        x = self.out_proj(x)  # Final projection to output logits
        return x

```

### 3. Concatenation (CLS | AVG | MAX)

This approach concatenates the [CLS] token embedding, mean-pooled embedding, and max-pooled embedding to create a richer representation.

```python

class EsmClassificationHead(nn.Module):
    """Head for sentence-level classification tasks, using [cls]| max | mean."""

    def __init__(self, config):
        super().__init__()
        self.dense = nn.Linear(config.hidden_size * 3, config.hidden_size)
        self.dropout = nn.Dropout(config.hidden_dropout_prob)
        self.out_proj = nn.Linear(config.hidden_size, config.num_labels)
        self.activation = nn.SiLU()
    def forward(self, features, attention_mask, **kwargs):
        cls = features[:, 0, :]  # take <s> token (equiv. to [CLS])
        # Expand attention_mask for broadcasting to match features' last dimension
        expanded_mask = attention_mask.unsqueeze(-1).expand_as(features)
        
        # Masked features - set padding tokens to 0
        masked_features = features * expanded_mask
        
        # Sum and count valid (non-padding) tokens to get the average
        sum_features = masked_features.sum(dim=1)
        
        non_padding_count = expanded_mask.sum(dim=1).clamp(min=1) # Clamp min to 1 to avoid division by zero
        avg_features = sum_features / non_padding_count  # Average only non-padding tokens

        # # Extract the max value of non-padding tokens
        max_features = masked_features.max(dim=1).values

        # concatenate the max, and average features
        x = torch.cat([cls, max_features, avg_features], dim=1) # Shape becomes [batch_size, 3 * hidden_size]
        x = self.dense(x)
        x = self.activation(x) #gelu(x) #torch.tanh(x) # #
        x = self.dropout(x)
        x = self.out_proj(x)  # Final projection to output logits
        return x

```

## Contributing

Feel free to open issues or pull requests for improvements and additional pooling strategies.

## License

This project is licensed under the MIT License.