library(tidyverse)
library(ggpubr)
library(ggsci)
library(ggh4x)

pdf_directory <- "plots/pdf"
png_directory <- "plots/png"



if (!dir.exists(pdf_directory)) {
  dir.create(pdf_directory, recursive = TRUE)
}

if (!dir.exists(png_directory)) {
  dir.create(png_directory, recursive = TRUE)
}

all_data <- read.csv("combined_output.csv") %>%
  filter(dataset != "promoter_tata") %>%
  data.frame()

all_data$group_data <- ifelse(
  startsWith(all_data$dataset, "H"), "Chromatin profiles",
  ifelse(
    startsWith(all_data$dataset, "enhan") | startsWith(all_data$dataset, "promo"), "Regulatory elements",
    ifelse(
      startsWith(all_data$dataset, "spli"), "Splicing",
      "Genome origin"
    )
  )
)

all_data$group_data <- factor(all_data$group_data,
  levels = c("Chromatin profiles", "Genome origin", "Splicing", "Regulatory elements")
)

# remove underscore from dataset names
all_data$dataset <- gsub("_", " ", all_data$dataset)
all_data$dataset <- gsub("splice sites donors", "donors", all_data$dataset)
all_data$dataset <- gsub("splice sites acceptors", "acceptors", all_data$dataset)
all_data$dataset <- gsub("splice sites all", "all", all_data$dataset)
# ones that does not start with H
all_data$dataset <- ifelse(
  !startsWith(all_data$dataset, "H"),
  stringr::str_to_sentence(all_data$dataset),
  all_data$dataset
)
all_data$dataset <- gsub("no tata", "(no TATA)", all_data$dataset)
all_data$dataset <- gsub("types", "(types)", all_data$dataset)

df_mcc <- all_data %>%
  filter(metric == "eval_mcc") %>%
  group_by(model, dataset, evaluation_set, metric) %>%
  summarise(avg_score = mean(score)) %>%
  ungroup() %>%
  spread(evaluation_set, avg_score) %>%
  mutate(difference = test - validation) %>%
  data.frame()



models_to_drop <- c(
  "seqLens_4096_512_46M-Me",
  "seqLens_4096_512_46M-Ms",
  "seqLens_4096_512_46M-Mp",
  "nt_50_avg", "nt_50_max", "nt_50_conv",
  "NT-100-LoRA",
  "seqLens_4096_512_89M-at-base-LoRA"
)


df_sum <- all_data %>%
  filter(
    metric %in% c(
      "eval_mcc", "eval_accuracy",
      "eval_f1_score", "eval_samples_per_second"
    ),
    !model %in% models_to_drop
  ) %>%
  group_by(model, dataset, evaluation_set, metric, group_data) %>%
  summarise(avg_score = mean(score)) %>%
  ungroup() %>%
  data.frame()

# find for esm in the model column
df_sum$model_type <- ifelse(startsWith(df_sum$model, "seqLens"),
  ifelse(grepl("esm", df_sum$model), "ESM", "DeBERTa"), "NT"
)

df_sum$model_info <- NA

df_sum[df_sum$model_type == "DeBERTa", "model_info"] <- unlist(strsplit(df_sum[df_sum$model_type == "DeBERTa", "model"], "_"))[c(F, F, F, T)]
df_sum[df_sum$model_type == "ESM", "model_info"] <- unlist(strsplit(df_sum[df_sum$model_type == "ESM", "model"], "_"))[c(F, F, F, F, T)]
df_sum[df_sum$model_type == "NT", "model_info"] <- toupper(unlist(strsplit(df_sum[df_sum$model_type == "NT", "model"], "-"))[c(F, F, F, T, F, F)])


df_sum$vocab_size <- NA
df_sum[df_sum$model_type == "DeBERTa", "vocab_size"] <- unlist(strsplit(df_sum[df_sum$model_type == "DeBERTa", "model"], "_"))[c(F, T, F, F)]
df_sum[df_sum$model_type == "ESM", "vocab_size"] <- unlist(strsplit(df_sum[df_sum$model_type == "ESM", "model"], "_"))[c(F, F, T, F, F)]
df_sum[df_sum$model_type == "NT", "vocab_size"] <- 4096

df_sum$model_info <- stringr::str_extract(df_sum$model_info, "^\\d{2,3}(?=M)")


df_sum$model_info <- as.numeric(df_sum$model_info)
df_sum$vocab_size <- as.numeric(df_sum$vocab_size)
df_sum$model_type <- as.factor(df_sum$model_type)

model_performance_all <- ggplot(
  df_sum %>%
    filter(metric == "eval_mcc"),
  aes(
    x = as.factor(vocab_size),
    y = avg_score,
    color = model_type,
    size = model_info,
    shape = evaluation_set
  )
) +
  geom_point(alpha = 0.5) +
  scale_size_continuous(range = c(0.2, 4)) +
  scale_shape_manual(values = c(17, 20)) +
  # facet_wrap( ~ dataset, scales = "free_y") +
  # scale_x_discrete(labels = vocab_size) +  # Ensure only specific vocab sizes are used
  labs(
    x = "Vocabulary Size",
    y = "MCC",
    color = "Model Type",
    size = "Model Size (M)",
    shape = "Evaluation Set"
  ) +
  guides(
    size = guide_legend(order = 1, nrow = 2), # Place size legend first
    shape = guide_legend(order = 2, nrow = 1), # Place shape legend second
    color = guide_legend(order = 3, nrow = 2) # Place color legend third in one row
  ) +
  coord_cartesian(clip = "off") +
  scale_size_continuous(range = c(0.2, 4)) +
  scale_color_manual(values = c("#0974c0", "#ecc21d", "#cd534d")) +
  omicsArt::theme_omicsEye() +
  ggh4x::facet_nested_wrap(~ group_data + dataset,
    strip = strip_nested(
      text_x = list(
        element_text(size = 8, face = "bold"),
        element_text(size = 7)
      ),
      by_layer_x = TRUE
    ),
    nest_line = element_line(linewidth = 0.2),
    scales = "free_y"
  )+theme(
    axis.text.x = element_text(size = 6, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 6), ,
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 7),
    axis.title = element_text(size = 8),
    axis.line = element_line(linewidth = 0.05),
    legend.position = c(0.82, 0.07), # Place the legend at the bottom
    legend.spacing.y = unit(0.1, "cm"),
    legend.spacing.x = unit(0.1, "cm"),
    legend.box = "vertical", # Arrange legends horizontally
    legend.box.just = "left", # Center the legend box
    axis.line.x = element_line(size = 0.2),
    axis.line.y = element_line(size = 0.2),
    axis.ticks.y = element_line(size = 0.2),
    axis.ticks.x = element_line(size = 0.2)
  )
ggsave(
  plot = model_performance_all, filename = paste0(pdf_directory, "/model_performance_all.pdf"),
  width = 7.2, height = 6
)
ggsave(
  plot = model_performance_all, filename = paste0(png_directory, "/model_performance_all.png"),
  width = 7.2, height = 6, bg = "white"
)


datasets <- c(
  "Plasmid", "Four kingdom", "Donors",
  "Promoter (no TATA)", "H2AFZ"
)
model_performance_subset = ggplot(
  df_sum %>%
    filter(metric == "eval_mcc",
           dataset %in% datasets),
  aes(
    x = as.factor(vocab_size),
    y = avg_score,
    color = model_type,
    size = model_info,
    shape = evaluation_set
  )
) +
  geom_point(alpha = 0.5) +
  scale_size_continuous(range = c(0.2, 4)) +
  scale_shape_manual(values=c(17, 20)) +
  facet_wrap( ~ dataset, scales = "free_y") +
  #scale_x_discrete(labels = vocab_size) +  # Ensure only specific vocab sizes are used
  labs(
    x = "Vocabulary Size",
    y = "MCC",
    color = "Model Type",
    size = "Model Size (M)",
    shape = "Evaluation Set"
  ) +
  coord_cartesian(clip = "off")+
  guides(
    size = guide_legend(order = 1, nrow = 2),     # Place size legend first
    shape = guide_legend(order = 2, nrow = 1),    # Place shape legend second
    color = guide_legend(order = 3, nrow = 1) # Place color legend third in one row
  )+
  scale_color_manual(values = c("#0974c0", "#ecc21d", "#cd534d"))+
  omicsArt::theme_omicsEye()+
  theme(
    
    legend.title = element_text(size = 6),
    legend.text = element_text(size = 5),
    legend.position = c(0.62,0.11),         # Place the legend at the bottom
    legend.spacing.x = unit(0.01,"cm"),
    legend.spacing.y = unit(0.01,"cm"),
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 8),
    
    legend.box = "vertical",          # Arrange legends horizontally
    legend.box.just = "left",          # Center the legend box,
    plot.margin = margin(t = 0,  # Top margin
                         r = 0,  # Right margin
                         b = 0,  # Bottom margin
                         l = 0) # Left margin
  ) 

ggsave(
  plot = model_performance_subset, filename = paste0(pdf_directory, "/model_performance_all_subset.pdf"),
  width = 4.3, height = 2.8
)
ggsave(
  plot = model_performance_subset, filename = paste0(png_directory, "/model_performance_all_subset.png"),
  width = 4.3, height = 2.8
)


vocab_4 <- ggplot(
  df_sum %>%
    filter(
      metric == "eval_mcc",
      vocab_size == 4096
    ),
  aes(
    x = model_info,
    y = avg_score,
    color = model_type,
    shape = evaluation_set
  )
) +
  geom_point(alpha = 0.5) +
    coord_cartesian(clip = "off") +
  # facet_wrap(~dataset, scales = "free_y") +
  geom_point(alpha = 0.5) +
  scale_shape_manual(values = c(17, 20)) +

  # scale_x_discrete(labels = vocab_size) +  # Ensure only specific vocab sizes are used
  labs(
    x = "Model Size (M)",
    y = "MCC",
    color = "Model Type",
    shape = "Evaluation Set"
  ) +
  guides(
    shape = guide_legend(order = 1, ncol = 1), # Place shape legend second
    color = guide_legend(order = 2, ncol = 1) # Place color legend third in one row
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  ) +
  scale_color_manual(values = c("#0974c0", "#ecc21d", "#cd534d")) +
  omicsArt::theme_omicsEye() +
  ggh4x::facet_nested_wrap(~ group_data + dataset,
    strip = strip_nested(
      text_x = list(
        element_text(size = 8, face = "bold"),
        element_text(size = 7)
      ),
      by_layer_x = TRUE
    ),
    nest_line = element_line(linewidth = 0.2),
    scales = "free_y"
  )+
  theme(
    axis.text.x = element_text(size = 6, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 6), ,
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 7),
    axis.title = element_text(size = 8),
    axis.line = element_line(linewidth = 0.05),
    legend.position = c(0.85, 0.08), # Place the legend at the bottom
    legend.spacing.y = unit(0.1, "cm"),
    legend.spacing.x = unit(0.1, "cm"),
    legend.box = "vertical", # Arrange legends horizontally
    legend.box.just = "left", # Center the legend box
    axis.line.x = element_line(size = 0.2),
    axis.line.y = element_line(size = 0.2),
    axis.ticks.y = element_line(size = 0.2),
    axis.ticks.x = element_line(size = 0.2)
  )

ggsave(
  plot = vocab_4, filename = paste0(pdf_directory, "/vocab_4.pdf"),
  width = 7.2, height = 5.76
)
ggsave(
  plot = vocab_4, filename = paste0(png_directory, "/vocab_4.png"),
  width = 7.2, height = 5.76, bg = "white"
)


all_data$group_data <- factor(all_data$group_data,
  levels = c("Chromatin profiles", "Regulatory elements",
  "Genome origin",
   "Splicing")
)

cl_models <- c(
  "nt_50_avg", "nt_50_max", "nt_50_conv",
  "nucleotide-transformer-v2-50m-multi-species"
)
df_cl <- all_data %>%
  filter(
    model %in% cl_models
  )
df_cl$model_n <- ifelse(df_cl$model == "nt_50_conv", "Concat",
  ifelse(df_cl$model == "nt_50_max", "Max",
    ifelse(df_cl$model == "nt_50_avg", "Mean",
      "[CLS]"
    )
  )
)
df_cl$model_n <- factor(df_cl$model_n, levels = c(
  "[CLS]", "Mean",
  "Max", "Concat"
))

p_cl_box = ggplot(
  df_cl %>%
    filter(
      metric == "eval_mcc",
      evaluation_set == "validation"
    ),
  aes(x = model_n, y = score, color = model_n)
) +
  geom_boxplot(lwd = 0.1, outlier.size = 0.5, outlier.alpha = 0.5) +  # Match outliers to jitter points
  geom_jitter(size = 0.5, alpha = 0.5) +  # Jittered points
  geom_point(
    data = df_cl %>%
      filter(metric == "eval_mcc", evaluation_set == "test"),
    aes(x = model_n, y = score, color = model_n),
    size = 1.5, # Customize point size
    shape = 17 # Customize point shape (e.g., triangle)
  ) +
  coord_cartesian(clip = "off") + # Ensure nothing gets clipped
 scale_color_manual(values = c("#309344", "#1871c2", "#f08c02", "#9c36b5")) +
  omicsArt::theme_omicsEye() +
  guides(color = guide_legend(nrow = 4)) +
  labs(
    y = "MCC",
    color = "Model Type"
  )+
  ggh4x::facet_nested_wrap(~ group_data + dataset,
    strip = strip_nested(
      text_x = list(
        element_text(size = 6, face = "bold"),
        element_text(size = 6)
      ),
      by_layer_x = TRUE
    ),
    nrow = 3,
    ncol = 7,
    nest_line = element_line(linewidth = 0.2),
    scales = "free_y"
  )+theme(
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    legend.position = c(.8, 0.15),
    axis.ticks.x = element_blank(),
    axis.line.x = element_line(size = 0.15),
    axis.line.y = element_line(size = 0.15),
    axis.ticks.y = element_line(size = 0.15)
  ) 

ggsave(
  plot = p_cl_box, filename = paste0(pdf_directory, "/cl_heads.pdf"),
  width = 7.2, height = 3.456
)
ggsave(
  plot = p_cl_box, filename = paste0(png_directory, "/cl_heads.png"),
  width = 7.2, height = 3.456, bg = "white"
)

# time cl
df_cl <- all_data %>%
  filter(
    model %in% cl_models,
    metric == "eval_samples_per_second"
  )

df_cl$model_n <- ifelse(df_cl$model == "nt_50_conv", "Concat",
  ifelse(df_cl$model == "nt_50_max", "Max",
    ifelse(df_cl$model == "nt_50_avg", "Mean",
      "[CLS]"
    )
  )
)

df_cl$model_n <- factor(df_cl$model_n, levels = c(
  "[CLS]", "Mean",
  "Max", "Concat"
))

# Calculate mean and standard error for each group
df_cl_summary <- df_cl %>%
  group_by(model, dataset, metric, group_data, model_n) %>%
  summarise(
    score_mean = mean(score),
    se = sd(score) / sqrt(n()),
    .groups = 'drop'
  ) %>%
  ungroup() %>%
  data.frame()

p_cl_time <- ggplot(
  df_cl_summary,
  aes(
    x = factor(dataset), 
    y = score_mean,
    fill = model_n
  )
) +
  # barplot
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  # Add error bars
  geom_errorbar(
    aes(ymin = score_mean - se, ymax = score_mean + se),
    position = position_dodge(width = 0.9),
    size = 0.2,
    width = 0.25
  ) +
  scale_fill_manual(values = c("#309344", "#1871c2", "#f08c02", "#9c36b5")) +
  omicsArt::theme_omicsEye() +
  theme(
    axis.text.x = element_text(angle=45, vjust=1, hjust=1),
    axis.title.x = element_blank(),
    panel.grid.major.y = element_line(colour = "gray", linewidth = 0.1),
    panel.grid.minor.y = element_line(colour = "gray", linewidth = 0.05),
    panel.grid.major.x = element_line(colour = "gray", linewidth = 0.05),
    axis.line.x = element_line(size = 0.15),
    axis.line.y = element_line(size = 0.15),
    axis.ticks.y = element_line(size = 0.15),
    axis.ticks.x = element_line(size = 0.15)

  ) +
  labs(
    y = "Average samples per second (± SE)",
    fill = "Pooling method"
  ) + 
  ggforce::facet_row(vars(group_data), scales = "free", space = "free") +
  theme(legend.position = "none")


ggsave(
  plot = p_cl_time, filename = paste0(pdf_directory, "/cl_heads_runtime.pdf"),
  width = 7.2, height = 1.728
)
ggsave(
  plot = p_cl_time, filename = paste0(png_directory, "/cl_heads_runtime.png"),
  width = 7.2, height = 1.728, bg = "white"
)

merged_cl = cowplot::plot_grid(p_cl_box, p_cl_time, nrow = 2,
                               labels = c("a", "b"),
                               rel_heights = c(2, 1),
                               label_size = 8,
                               label_x = 0.01,
                               label_y = c(1,1.02))
                            
ggsave(
  plot = merged_cl, filename = paste0(pdf_directory, "/cl_heads_merged.pdf"),
  width = 7.2, height = 5.184
)
ggsave(
  plot = merged_cl, filename = paste0(png_directory, "/cl_heads_merged.png"),
  width = 7.2, height = 5.184, bg = "white"
)

# cpt
cpt_models <- c(
  "seqLens_4096_512_46M",
  "seqLens_4096_512_46M-Me",
  "seqLens_4096_512_46M-Ms",
  "seqLens_4096_512_46M-Mp"
)
df_cpt <- all_data %>%
  filter(
    model %in% cpt_models,
    metric == "eval_mcc"
  )
df_cpt$model <- factor(df_cpt$model, levels = c(
  "seqLens_4096_512_46M-Me",
  "seqLens_4096_512_46M-Mp",
  "seqLens_4096_512_46M-Ms",
  "seqLens_4096_512_46M"
))


p_cpt = ggplot(
  df_cpt %>%
    filter(
      metric == "eval_mcc",
      evaluation_set == "validation"
    ),
  aes(x = model, y = score, color = model)
) +
  geom_boxplot(lwd = 0.1, outlier.size = 0.5, outlier.alpha = 0.5) +  # Match outliers to jitter points
  geom_jitter(size = 0.5, alpha = 0.5) +  # Jittered points
  geom_point(
    data = df_cpt %>%
      filter(metric == "eval_mcc", evaluation_set == "test"),
    aes(x = model, y = score, color = model),
    size = 1.5, # Customize point size
    shape = 17 # Customize point shape (e.g., triangle)
  ) +
  coord_cartesian(clip = "off") + # Ensure nothing gets clipped
 scale_color_manual(values = c("#2e2a2b", "#cf4e9c", "#8b57a2", "#358db9")) +
  omicsArt::theme_omicsEye() +
  guides(color = guide_legend(nrow = 4)) +
  labs(
    y = "MCC",
    color = "Model Type"
  )+
  ggh4x::facet_nested_wrap(~ group_data + dataset,
    strip = strip_nested(
      text_x = list(
        element_text(size = 6, face = "bold"),
        element_text(size = 6)
      ),
      by_layer_x = TRUE
    ),
    nrow = 3,
    ncol = 7,
    nest_line = element_line(linewidth = 0.2),
    scales = "free_y"
  )+theme(
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    legend.position = c(.8, 0.15),
    axis.ticks.x = element_blank(),
    axis.line.x = element_line(size = 0.15),
    axis.line.y = element_line(size = 0.15),
    axis.ticks.y = element_line(size = 0.15)
  ) 


ggsave(
  plot = p_cpt, filename = paste0(pdf_directory, "/continual_pretraining.pdf"),
  width = 7.2, height = 3.456
)
ggsave(
  plot = p_cpt, filename = paste0(png_directory, "/continual_pretraining.png"),
  width = 7.2, height = 3.456
)

#read the token embeddings
token_embeddings <- read.csv("token_embeddings.csv") %>%
  data.frame()


token_embeddings$token_len = nchar(token_embeddings$token)
token_embeddings$model = ifelse(token_embeddings$model == "base", "seqLens_4096_512_46M",
  ifelse(token_embeddings$model == "Ms", "seqLens_4096_512_46M-Ms",
    ifelse(token_embeddings$model == "Mp", "seqLens_4096_512_46M-Mp",
      "seqLens_4096_512_46M-Me"
    ) 
)
)

token_embeddings$model <- factor(token_embeddings$model, levels = c(
  "seqLens_4096_512_46M-Me",
  "seqLens_4096_512_46M-Mp",
  "seqLens_4096_512_46M-Ms",
  "seqLens_4096_512_46M"
))

token_embeddings_plot = ggplot(token_embeddings, aes(x = x, y = y, color = factor(token_len))) +
  geom_point(size = 0.2, alpha = 0.2) +
  coord_cartesian(clip = "off") +  # Ensure nothing gets clipped
  facet_wrap(~model, nrow = 1) +
  
  # Annotate the points where token == "abcd"
  geom_text(
    data = token_embeddings %>% filter(token %in% c("aaagaa", "gaagcga", "gacgacca")),
    aes(label = toupper(token)), 
    vjust = 1.4, hjust = 0.3,
    color = "black", fontface = "bold", size = 2
  ) +
  # add a larger circle highlight the points
  geom_point(
    data = token_embeddings %>% filter(token %in% c("aaagaa", "gaagcga", "gacgacca")),
    aes(x = x, y = y), color = "purple", shape = 21,
    size = 1.5
  ) +
  scale_color_simpsons() +
  omicsArt::theme_omicsEye() +
  guides(color = guide_legend(override.aes = list(alpha = 1, size = 1), nrow=1)) +
  labs(
    y = "UMAP 2",
    x = "UMAP 1",
    color = "Token Length"
  )+
  theme(
    legend.position = "top",
    axis.line.x = element_line(size = 0.15),
    axis.line.y = element_line(size = 0.15),
    axis.ticks.y = element_line(size = 0.15),
    axis.title = element_text(size = 6)
  )
ggsave(
  plot = token_embeddings_plot, filename = paste0(pdf_directory, "/token_embeddings.pdf"),
  width = 7.2, height = 2
)

ggsave(
  plot = token_embeddings_plot, filename = paste0(png_directory, "/token_embeddings.png"),
  width = 7.2, height = 2, bg = "white"
)


# distance box-plot
embedding_distances <- read.csv("token_distances.csv") %>%
  select(-token) %>%
  gather(key = "model", value = "value") %>%
  data.frame()


embedding_distances$model = ifelse(embedding_distances$model == "base", "seqLens_4096_512_46M",
  ifelse(embedding_distances$model == "Ms", "seqLens_4096_512_46M-Ms",
    ifelse(embedding_distances$model == "Mp", "seqLens_4096_512_46M-Mp",
      "seqLens_4096_512_46M-Me"
    ) 
)
)

embedding_distances$model <- factor(embedding_distances$model, levels = c(
  "seqLens_4096_512_46M-Me",
  "seqLens_4096_512_46M-Mp",
  "seqLens_4096_512_46M-Ms",
  "seqLens_4096_512_46M"
))

distance_box_plot = ggplot(
  embedding_distances ,
  aes(x = model, y = value, color = model)
) +
  geom_boxplot(lwd = 0.2, 
  outlier.size = 0.1, 
  outlier.alpha = 0.02) +  # Match outliers to jitter points
  geom_jitter(size = 0.1, alpha = 0.02) +  # Jittered points
  coord_cartesian(clip = "off") + # Ensure nothing gets clipped
 scale_color_manual(values = c("#2e2a2b", "#cf4e9c", "#8b57a2", "#358db9")) +
  omicsArt::theme_omicsEye() +
  guides(color = guide_legend(nrow = 4)) +
  labs(
    y = "Distance from 46M model",
    color = "Model Type",
    x = "Models"
  )+
  scale_x_discrete(labels = c(expression("||M - Me||"[2]),
                            expression("||M - Mp||"[2]),
                            expression("||M - Ms||"[2])))+
  theme(
    # axis.text.x = element_blank(),
    axis.title = element_text(size = 6),
    legend.position = "none",
    axis.ticks.x = element_blank(),
    axis.line.x = element_line(size = 0.15),
    axis.line.y = element_line(size = 0.15),
    axis.ticks.y = element_line(size = 0.15)
  ) 


ggsave(
  plot = distance_box_plot, 
  filename = paste0(pdf_directory, "/embedding_distances.pdf"),
  width = 2.4, height = 2.4
)
#combined cpt (first row), token_embeddings_plot and distance_box_plot (second row)
bottom_row = cowplot::plot_grid(token_embeddings_plot, distance_box_plot, nrow = 1,
                                labels = c("b", "c"),
                                label_size = 8,
                                label_x = c(0.01, -0.03),
                                label_y = 1,
                               rel_heights = c(1, 1),
                               rel_widths = c(3, 1))
merged_cpt = cowplot::plot_grid(p_cpt, bottom_row, nrow = 2,
                                labels = c("a", ""),
                                rel_heights = c(2, 1),
                                label_size = 8,
                                label_x = 0.01,
                                label_y = c(1,1.02))

ggsave(
  plot = merged_cpt, filename = paste0(pdf_directory, "/cpt_embeddings.pdf"),
  width = 7.2, height = 5.5
)
ggsave(
  plot = merged_cpt, filename = paste0(png_directory, "/cpt_embeddings.png"),
  width = 7.2, height = 5.5, bg = "white")

# deberta architecture
deb_models <- c(
  "seqLens_4096_512_15M",
  "seqLens_4096_512_23M-at-xsmall",
  "seqLens_4096_512_27M",
  "seqLens_4096_512_46M",
  "seqLens_4096_512_47M-at-small",
  "seqLens_4096_512_89M",
  "seqLens_4096_512_89M-at-base"
)
df_deb <- all_data %>%
  filter(
    model %in% deb_models
  )

df_deb$model <- factor(df_deb$model, levels = deb_models)

p_4096_deberta = ggplot(
  df_deb %>%
    filter(
      metric == "eval_mcc",
      evaluation_set == "validation"
    ),
  aes(x = model, y = score, color = model)
) +
  geom_boxplot(lwd = 0.1, outlier.size = 0.5, outlier.alpha = 0.5) +  # Match outliers to jitter points
  geom_jitter(size = 0.5, alpha = 0.5) +  # Jittered points
  geom_point(
    data = df_deb %>%
      filter(metric == "eval_mcc", evaluation_set == "test"),
    aes(x = model, y = score, color = model),
    size = 1.5, # Customize point size
    shape = 17 # Customize point shape (e.g., triangle)
  ) +
  coord_cartesian(clip = "off") + # Ensure nothing gets clipped
 scale_color_jco() +
  omicsArt::theme_omicsEye() +
  guides(color = guide_legend(ncol = 1)) +
  labs(
    y = "MCC",
    color = "Model Type"
  )+
  ggh4x::facet_nested_wrap(~ group_data + dataset,
    strip = strip_nested(
      text_x = list(
        element_text(size = 6, face = "bold"),
        element_text(size = 6)
      ),
      by_layer_x = TRUE
    ),
    nrow = 3,
    ncol = 7,
    nest_line = element_line(linewidth = 0.2),
    scales = "free_y"
  )+theme(
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    legend.position = c(.75, 0.15),
    axis.ticks.x = element_blank(),
    axis.line.x = element_line(size = 0.15),
    axis.line.y = element_line(size = 0.15),
    axis.ticks.y = element_line(size = 0.15)
  ) 

ggsave(
  plot = p_4096_deberta, filename = paste0(pdf_directory, "/4096_deberta.pdf"),
  width = 7.2, height = 3.456
)
ggsave(
  plot = p, filename = paste0(png_directory, "/4096_deberta.png"),
  width = 7.2, height = 3.456, bg = "white"
)

# time deberta
df_deb <- all_data %>%
  filter(
    model %in% deb_models,
    metric == "eval_samples_per_second"
  )

df_deb$model <- factor(df_deb$model, levels = deb_models)

# Calculate mean and standard error for each group
df_deb_summary <- df_deb %>%
  group_by(model, dataset, metric, group_data) %>%
  summarise(
    score_mean = mean(score),
    se = sd(score) / sqrt(n()),
    .groups = 'drop'
  ) %>%
  ungroup() %>%
  data.frame()

p_deb_time <- ggplot(
  df_deb_summary,
  aes(
    x = factor(dataset), 
    y = score_mean,
    fill = model
  )
) +
  # barplot
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  # Add error bars
  geom_errorbar(
    aes(ymin = score_mean - se, ymax = score_mean + se),
    position = position_dodge(width = 0.9),
    size = 0.15,
    width = 0.25
  ) +
  scale_fill_jco() +
  omicsArt::theme_omicsEye() +
  theme(
    axis.text.x = element_text(angle=45, vjust=1, hjust=1),
    axis.title.x = element_blank(),
    panel.grid.major.y = element_line(colour = "gray", linewidth = 0.1),
    panel.grid.minor.y = element_line(colour = "gray", linewidth = 0.05),
    panel.grid.major.x = element_line(colour = "gray", linewidth = 0.05),
    axis.line.x = element_line(size = 0.15),
    axis.line.y = element_line(size = 0.15),
    axis.ticks.y = element_line(size = 0.15),
    axis.ticks.x = element_line(size = 0.15)
  ) +
  labs(
    y = "Average samples per second (± SE)",
    fill = "Model"
  )

p_deb_time <- p_deb_time +
  ggforce::facet_row(vars(group_data), scales = "free", space = "free") +
  theme(legend.position = "none")

ggsave(
  plot = p_deb_time, filename = paste0(pdf_directory, "/deberta_runtime.pdf"),
  width = 7.2, height = 1.728
)
ggsave(
  plot = p_deb_time, filename = paste0(png_directory, "/deberta_runtime.png"),
  width = 7.2, height = 1.728, bg = "white"
)

# combined
merged_deb = cowplot::plot_grid(p_4096_deberta, p_deb_time, nrow = 2,
                               labels = c("a", "b"),
                               rel_heights = c(2, 1),
                               label_size = 8,
                               label_x = 0.01,
                               label_y = c(1,1.02))

ggsave(
  plot = merged_deb, filename = paste0(pdf_directory, "/deberta_merged.pdf"),
  width = 7.2, height = 5.184
)
ggsave(
  plot = merged_deb, filename = paste0(png_directory, "/deberta_merged.png"),
  width = 7.2, height = 5.184, bg = "white"
)


## seq models
seq_models <- all_data %>%
  filter(
    grepl("seqLens", model),
    !grepl("esm", model)
  )
# find for esm in the model column
seq_models$model_type <- "DeBERTa"

seq_models$model_info <- NA

seq_models$model_info <- unlist(strsplit(seq_models$model, "_"))[c(F, F, F, T)]

seq_models$vocab_size <- NA
seq_models$vocab_size <- unlist(strsplit(seq_models$model, "_"))[c(F, T, F, F)]

seq_models$model_info <- stringr::str_extract(seq_models$model_info, "^\\d{2,3}(?=M)")


seq_models$model_info <- as.numeric(seq_models$model_info)
seq_models$vocab_size <- as.numeric(seq_models$vocab_size)
seq_models$model_type <- as.factor(seq_models$model_type)
seq_models_simple <- seq_models %>%
  filter(!grepl("at|M-M", model)) %>%
  left_join(
    seq_models %>%
      filter(!grepl("at|M-M", model)) %>%
      group_by(vocab_size, model_info) %>%
      summarise(n = n()) %>%
      ungroup() %>%
      group_by(vocab_size) %>%
      mutate(rank = rank(model_info)) %>%
      # filter(rank<4) %>%
      select(model_info, vocab_size, rank) %>%
      ungroup(),
    by = c("model_info", "vocab_size")
  )
# compare model_size
sm_models <- c(
  "seqLens_4096_512_15M",
  "seqLens_8192_512_17M",
  "seqLens_16384_512_22M",
  "seqLens_32768_512_30M",
  "seqLens_65536_512_47M"
)

for (i in 1:3) {
  tmp <- seq_models_simple %>% filter(rank == i, metric == "eval_mcc")
  level_df <- tmp %>%
    group_by(model, vocab_size) %>%
    summarise(n = n()) %>%
    select(model, vocab_size) %>%
    arrange(vocab_size)
  tmp$model <- factor(tmp$model, levels = level_df$model[order(level_df$vocab_size)])

  p_box <- ggplot(
  tmp %>%
    filter(
      metric == "eval_mcc",
      evaluation_set == "validation"
    ),
  aes(x = model, y = score, color = model)
) +
  geom_boxplot(lwd = 0.1, outlier.size = 0.5, outlier.alpha = 0.5) +  # Match outliers to jitter points
  geom_jitter(size = 0.5, alpha = 0.5) +  # Jittered points
  geom_point(
    data = tmp %>%
      filter(metric == "eval_mcc", evaluation_set == "test"),
    aes(x = model, y = score, color = model),
    size = 1.5, # Customize point size
    shape = 17 # Customize point shape (e.g., triangle)
  ) +
  coord_cartesian(clip = "off") + # Ensure nothing gets clipped
 scale_color_jco() +
  omicsArt::theme_omicsEye() +
  guides(color = guide_legend(ncol = 1)) +
  labs(
    y = "MCC",
    color = "Model Type"
  )+
  ggh4x::facet_nested_wrap(~ group_data + dataset,
    strip = strip_nested(
      text_x = list(
        element_text(size = 6, face = "bold"),
        element_text(size = 6)
      ),
      by_layer_x = TRUE
    ),
    nrow = 3,
    ncol = 7,
    nest_line = element_line(linewidth = 0.2),
    scales = "free_y"
  )+theme(
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    legend.position = c(.75, 0.15),
    axis.ticks.x = element_blank(),
    axis.line.x = element_line(size = 0.15),
    axis.line.y = element_line(size = 0.15),
    axis.ticks.y = element_line(size = 0.15)
  ) 

  ggsave(
    plot = p_box, filename = paste0(pdf_directory, "/deberta_simple_", i, ".pdf"),
    width = 7.2, height = 3.456
  )
  ggsave(
    plot = p_box, filename = paste0(png_directory, "/deberta_simple_", i, ".png"),
    width = 7.2, height = 3.456, bg = "white"
  )

  # time  
  tmp <- seq_models_simple %>% filter(rank == i, metric == "eval_samples_per_second")
  level_df <- tmp %>%
    group_by(model, vocab_size) %>%
    summarise(n = n()) %>%
    select(model, vocab_size) %>%
    arrange(vocab_size)
  tmp$model <- factor(tmp$model, levels = level_df$model[order(level_df$vocab_size)])

  # Calculate mean and standard error for each group
  tmp_summary <- tmp %>%
    group_by(model, dataset, metric, group_data) %>%
    summarise(
      score_mean = mean(score),
      se = sd(score) / sqrt(n()),
      .groups = 'drop'
    ) %>%
    ungroup() %>%
    data.frame()

  p_time <- ggplot(
    tmp_summary,
    aes(
      x = factor(dataset), 
      y = score_mean,
      fill = model
    )
  ) +
    # barplot
    geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
    # Add error bars
    geom_errorbar(
      aes(ymin = score_mean - se, ymax = score_mean + se),
      position = position_dodge(width = 0.9),
      size = 0.15,
      width = 0.25
    ) +
    scale_fill_jco() +
    omicsArt::theme_omicsEye() +
    theme(
      axis.text.x = element_text(angle=45, vjust=1, hjust=1),
      axis.title.x = element_blank(),
      panel.grid.major.y = element_line(colour = "gray", linewidth = 0.1),
      panel.grid.minor.y = element_line(colour = "gray", linewidth = 0.05),
      panel.grid.major.x = element_line(colour = "gray", linewidth = 0.05),
      axis.line.x = element_line(size = 0.15),
      axis.line.y = element_line(size = 0.15),
      axis.ticks.y = element_line(size = 0.15),
      axis.ticks.x = element_line(size = 0.15)
    ) +
    labs(
      y = "Average samples per second (± SE)",
      fill = "Model"
    )

  p_time <- p_time +
    ggforce::facet_row(vars(group_data), scales = "free", space = "free") +
    theme(legend.position = "none")

  ggsave(
    plot = p_time, filename = paste0(pdf_directory, "/deberta_simple_time_", i, ".pdf"),
    width = 7.2, height = 1.728
  )
  ggsave(
    plot = p_time, filename = paste0(png_directory, "/deberta_simple_time_", i, ".png"),
    width = 7.2, height = 1.728, bg = "white"
  )

  merged_deb = cowplot::plot_grid(p_box, p_time, nrow = 2,
                                 labels = c("a", "b"),
                                 rel_heights = c(2, 1),
                                 label_size = 8,
                                 label_x = 0.01,
                                 label_y = c(1,1.02))

  ggsave(
    plot = merged_deb, filename = paste0(pdf_directory, "/deberta_simple_merged_", i, ".pdf"),
    width = 7.2, height = 5.184
  )
  ggsave(
    plot = merged_deb, filename = paste0(png_directory, "/deberta_simple_merged_", i, ".png"),
    width = 7.2, height = 5.184, bg = "white"
  )

}
# nt vs esm vs deberta
tp_models <- c(
  "seqLens_4096_512_47M-at-small",
  "seqLens_esm_4096_512_55M",
  "seqLens_4096_512_89M-at-base",
  "seqLens_4096_512_89M-at-base-multi",
  "seqLens_esm_4096_512_97M",
  "nucleotide-transformer-v2-100m-multi-species",
  "nucleotide-transformer-v2-50m-multi-species"
)

df_tp <- all_data %>%
  filter(
    model %in% tp_models
  )

df_tp$model <- ifelse(df_tp$model == "nucleotide-transformer-v2-100m-multi-species", "NT-100",
  ifelse(df_tp$model == "nucleotide-transformer-v2-50m-multi-species", "NT-50",
    df_tp$model
  )
)
df_tp$model <- factor(df_tp$model,
  levels = c(
    "seqLens_esm_4096_512_55M",
    "NT-50",
    "seqLens_4096_512_47M-at-small",
    "seqLens_esm_4096_512_97M",
    "NT-100",
    "seqLens_4096_512_89M-at-base",
    "seqLens_4096_512_89M-at-base-multi"
  )
)

p_comparison = ggplot(
  df_tp %>%
    filter(
      metric == "eval_mcc",
      evaluation_set == "validation"
    ),
  aes(x = model, y = score, color = model)
) +
  geom_boxplot(lwd = 0.1, outlier.size = 0.5, outlier.alpha = 0.5) +  # Match outliers to jitter points
  geom_jitter(size = 0.5, alpha = 0.5) +  # Jittered points
  geom_point(
    data = df_tp %>%
      filter(metric == "eval_mcc", evaluation_set == "test"),
    aes(x = model, y = score, color = model),
    size = 1.5, # Customize point size
    shape = 17 # Customize point shape (e.g., triangle)
  ) +
  coord_cartesian(clip = "off") + # Ensure nothing gets clipped
 scale_color_manual(values = c("#F6E08A", "#E6A29E", "#75B3DF", "#ecc21d", "#cd534d", "#0974c0", "#05395e")) +
  omicsArt::theme_omicsEye() +
  guides(color = guide_legend(ncol = 1)) +
  labs(
    y = "MCC",
    color = "Model Type"
  )+
  ggh4x::facet_nested_wrap(~ group_data + dataset,
    strip = strip_nested(
      text_x = list(
        element_text(size = 6, face = "bold"),
        element_text(size = 6)
      ),
      by_layer_x = TRUE
    ),
    nrow = 3,
    ncol = 7,
    nest_line = element_line(linewidth = 0.2),
    scales = "free_y"
  )+theme(
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    legend.position = c(.75, 0.15),
    axis.ticks.x = element_blank(),
    axis.line.x = element_line(size = 0.15),
    axis.line.y = element_line(size = 0.15),
    axis.ticks.y = element_line(size = 0.15)
  )

ggsave(
  plot = p_comparison, filename = paste0(pdf_directory, "/nt_esm_vs_deberta.pdf"),
  width = 7.2, height = 3.456
)
ggsave(
  plot = p_comparison, filename = paste0(png_directory, "/nt_esm_vs_deberta.png"),
  width = 7.2, height = 3.456, bg = "white"
)

# time
# Calculate mean and standard error for each group
df_tp_summary <- df_tp %>%
filter(metric == "eval_samples_per_second") %>%
  group_by(model, dataset, metric, group_data) %>%
  summarise(
    score_mean = mean(score),
    se = sd(score) / sqrt(n()),
    .groups = 'drop'
  ) %>%
  ungroup() %>%
  data.frame()

p_time <- ggplot(
  df_tp_summary,
  aes(
    x = factor(dataset), 
    y = score_mean,
    fill = model
  )
) +
  # barplot
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  # Add error bars
  geom_errorbar(
    aes(ymin = score_mean - se, ymax = score_mean + se),
    position = position_dodge(width = 0.9),
    size = 0.15,
    width = 0.25
  ) +
  scale_fill_manual(values = c("#F6E08A", "#E6A29E", "#75B3DF", "#ecc21d", "#cd534d", "#0974c0", "#05395e")) +
  omicsArt::theme_omicsEye() +
  theme(
    axis.text.x = element_text(angle=45, vjust=1, hjust=1),
    axis.title.x = element_blank(),
    panel.grid.major.y = element_line(colour = "gray", linewidth = 0.1),
    panel.grid.minor.y = element_line(colour = "gray", linewidth = 0.05),
    panel.grid.major.x = element_line(colour = "gray", linewidth = 0.05),
    axis.line.x = element_line(size = 0.15),
    axis.line.y = element_line(size = 0.15),
    axis.ticks.y = element_line(size = 0.15),
    axis.ticks.x = element_line(size = 0.15)
  ) +
  labs(
    y = "Average samples per second (± SE)",
    fill = "Model"
  )

p_time <- p_time +
  ggforce::facet_row(vars(group_data), scales = "free", space = "free") +
  theme(legend.position = "none")

ggsave(
  plot = p_time, filename = paste0(pdf_directory, "/nt_esm_vs_deberta_time.pdf"),
  width = 7.2, height = 1.728
)

ggsave(
  plot = p_time, filename = paste0(png_directory, "/nt_esm_vs_deberta_time.png"),
  width = 7.2, height = 1.728, bg = "white"
)

merged_tp = cowplot::plot_grid(p_comparison, p_time, nrow = 2,
                               labels = c("a", "b"),
                               rel_heights = c(2, 1),
                               label_size = 8,
                               label_x = 0.01,
                               label_y = c(1,1.02))

ggsave(
  plot = merged_tp, filename = paste0(pdf_directory, "/nt_esm_vs_deberta_merged.pdf"),
  width = 7.2, height = 5.184
)

ggsave(
  plot = merged_tp, filename = paste0(png_directory, "/nt_esm_vs_deberta_merged.png"),
  width = 7.2, height = 5.184, bg = "white")

# LoRA
lora_models <- c(
  "seqLens_4096_512_89M-at-base",
  "seqLens_4096_512_89M-at-base-LoRA",
  "nucleotide-transformer-v2-100m-multi-species",
  "NT-100-LoRA"
)

df_lora <- all_data %>%
  filter(
    model %in% lora_models,
    metric == "eval_mcc"
  )

df_lora$model <- ifelse(df_lora$model == "nucleotide-transformer-v2-100m-multi-species", "NT-100",
df_lora$model
  )

df_lora$model <- factor(df_lora$model,
  levels = c(
    "seqLens_4096_512_89M-at-base",
  "seqLens_4096_512_89M-at-base-LoRA",
  "NT-100",
  "NT-100-LoRA"
  )
)

p_lora = ggplot(
  df_lora %>%
    filter(
      metric == "eval_mcc",
      evaluation_set == "validation"
    ),
  aes(x = model, y = score, color = model)
) +
  geom_boxplot(lwd = 0.1, outlier.size = 0.5, outlier.alpha = 0.5) +  # Match outliers to jitter points
  geom_jitter(size = 0.5, alpha = 0.5) +  # Jittered points
  geom_point(
    data = df_lora %>%
      filter(metric == "eval_mcc", evaluation_set == "test"),
    aes(x = model, y = score, color = model),
    size = 1.5, # Customize point size
    shape = 17 # Customize point shape (e.g., triangle)
  ) +
  coord_cartesian(clip = "off") + # Ensure nothing gets clipped
 scale_color_manual(values = c("#0974c0", "#6b95b3", "#cd534d", "#d4a3a1")) +
  omicsArt::theme_omicsEye() +
  guides(color = guide_legend(ncol = 1)) +
  labs(
    y = "MCC",
    color = "Model Type"
  )+
  ggh4x::facet_nested_wrap(~ group_data + dataset,
    strip = strip_nested(
      text_x = list(
        element_text(size = 6, face = "bold"),
        element_text(size = 6)
      ),
      by_layer_x = TRUE
    ),
    nrow = 3,
    ncol = 7,
    nest_line = element_line(linewidth = 0.2),
    scales = "free_y"
  )+theme(
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    legend.position = c(.75, 0.15),
    axis.ticks.x = element_blank(),
    axis.line.x = element_line(size = 0.15),
    axis.line.y = element_line(size = 0.15),
    axis.ticks.y = element_line(size = 0.15)
  )

ggsave(
  plot = p_lora, filename = paste0(pdf_directory, "/lora_boxplot.pdf"),
  width = 7.2, height = 3.456
)
ggsave(
  plot = p_lora, filename = paste0(png_directory, "/lora_boxplot.png"),
  width = 7.2, height = 3.456, bg = "white"
)
# time
df_lora <- all_data %>%
  filter(
    model %in% lora_models,
    metric == "eval_samples_per_second"
  )
df_lora$model <- ifelse(df_lora$model == "nucleotide-transformer-v2-100m-multi-species", "NT-100",
df_lora$model
  )
df_lora$model <- factor(df_lora$model,
  levels = c(
    "seqLens_4096_512_89M-at-base",
  "seqLens_4096_512_89M-at-base-LoRA",
  "NT-100",
  "NT-100-LoRA"
  )
)
# Calculate mean and standard error for each group
df_lora_summary <- df_lora %>%
  group_by(model, dataset, metric, group_data) %>%
  summarise(
    score_mean = mean(score),
    se = sd(score) / sqrt(n()),
    .groups = 'drop'
  ) %>%
  ungroup() %>%
  data.frame()

p_lora_time <- ggplot(
  df_lora_summary,
  aes(
    x = factor(dataset), 
    y = score_mean,
    fill = model
  )
) +
  # barplot
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  # Add error bars
  geom_errorbar(
    aes(ymin = score_mean - se, ymax = score_mean + se),
    position = position_dodge(width = 0.9),
    size = 0.15,
    width = 0.25
  ) +
  scale_fill_manual(values = c("#0974c0", "#6b95b3", "#cd534d", "#d4a3a1")) +
  omicsArt::theme_omicsEye() +
  theme(
    axis.text.x = element_text(angle=45, vjust=1, hjust=1),
    axis.title.x = element_blank(),
    panel.grid.major.y = element_line(colour = "gray", linewidth = 0.1),
    panel.grid.minor.y = element_line(colour = "gray", linewidth = 0.05),
    panel.grid.major.x = element_line(colour = "gray", linewidth = 0.05),
    axis.line.x = element_line(size = 0.15),
    axis.line.y = element_line(size = 0.15),
    axis.ticks.y = element_line(size = 0.15),
    axis.ticks.x = element_line(size = 0.15)
  ) +
  labs(
    y = "Average samples per second (± SE)",
    fill = "Model"
  )

p_lora_time <- p_lora_time +
  ggforce::facet_row(vars(group_data), scales = "free", space = "free") +
  theme(legend.position = "none")

ggsave(
  plot = p_lora_time, filename = paste0(pdf_directory, "/lora_time.pdf"),
  width = 7.2, height = 1.728
)

ggsave(
  plot = p_lora_time, filename = paste0(png_directory, "/lora_time.png"),
  width = 7.2, height = 1.728, bg = "white"
)

merged_lora = cowplot::plot_grid(p_lora, p_lora_time, nrow = 2,
                               labels = c("a", "b"),
                               rel_heights = c(2, 1),
                               label_size = 8,
                               label_x = 0.01,
                               label_y = c(1,1.02))

ggsave( 
  plot = merged_lora, filename = paste0(pdf_directory, "/lora_merged.pdf"),
  width = 7.2, height = 5.184
)

ggsave(
  plot = merged_lora, filename = paste0(png_directory, "/lora_merged.png"),
  width = 7.2, height = 5.184, bg = "white"
)
