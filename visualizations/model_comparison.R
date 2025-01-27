library(tidyverse)
library(ggpubr)
library(ggsci)


pdf_directory = "plots/pdf" 
png_directory = "plots/png" 



if (!dir.exists(pdf_directory)) { 
  dir.create(pdf_directory,recursive = TRUE)
  }

if (!dir.exists(png_directory)) { 
  dir.create(png_directory,recursive = TRUE)
}

all_data = read.csv("combined_output.csv") %>%
  filter(dataset != "promoter_tata") %>% 
  data.frame()

df_mcc = all_data %>% 
  filter(metric == "eval_mcc") %>% 
  group_by(model, dataset, evaluation_set, metric) %>% 
  summarise(avg_score = mean(score)) %>% 
  ungroup() %>% 
  spread(evaluation_set, avg_score) %>% 
  mutate(difference = test - validation) %>% 
  data.frame()



models_to_drop = c(
  "seqsight_4096_512_46M-Me",
  "seqsight_4096_512_46M-Ms",
  "seqsight_4096_512_46M-Mp",
  "nt_50_avg", "nt_50_max", "nt_50_conv"
)


df_sum = all_data %>% 
  filter(metric %in% c("eval_mcc", "eval_accuracy", 
                       "eval_f1_score", "eval_samples_per_second"),
         !model %in% models_to_drop) %>% 
  group_by(model, dataset, evaluation_set, metric) %>% 
  summarise(avg_score = mean(score)) %>% 
  ungroup() %>% 
  data.frame()

# find for esm in the model column
df_sum$model_type = ifelse(startsWith(df_sum$model, "seqsight"),
                           ifelse(grepl("esm", df_sum$model), "ESM", "DeBERTa"), "NT")

df_sum$model_info = NA

df_sum[df_sum$model_type == "DeBERTa", "model_info"] = unlist(strsplit(df_sum[df_sum$model_type == "DeBERTa", "model"], "_"))[c(F,F,F,T)]
df_sum[df_sum$model_type == "ESM", "model_info"] = unlist(strsplit(df_sum[df_sum$model_type == "ESM", "model"], "_"))[c(F,F,F,F,T)]
df_sum[df_sum$model_type == "NT", "model_info"] = toupper(unlist(strsplit(df_sum[df_sum$model_type == "NT", "model"], "-"))[c(F,F,F,T,F,F)])


df_sum$vocab_size = NA
df_sum[df_sum$model_type == "DeBERTa", "vocab_size"] = unlist(strsplit(df_sum[df_sum$model_type == "DeBERTa", "model"], "_"))[c(F,T,F,F)]
df_sum[df_sum$model_type == "ESM", "vocab_size"] = unlist(strsplit(df_sum[df_sum$model_type == "ESM", "model"], "_"))[c(F,F,T,F,F)]
df_sum[df_sum$model_type == "NT", "vocab_size"] = 4096

df_sum$model_info <- stringr::str_extract(df_sum$model_info, "^\\d{2,3}(?=M)")


df_sum$model_info = as.numeric(df_sum$model_info)
df_sum$vocab_size = as.numeric(df_sum$vocab_size)
df_sum$model_type = as.factor(df_sum$model_type)

model_performance_all = ggplot(
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
  guides(
    size = guide_legend(order = 1, nrow = 1),     # Place size legend first
    shape = guide_legend(order = 2, nrow = 1),    # Place shape legend second
    color = guide_legend(order = 3, nrow = 1) # Place color legend third in one row
  )+
  scale_color_manual(values = c("#0974c0", "#ecc21d", "#cd534d"))+
  omicsArt::theme_omicsEye()+
  theme(
    legend.position = c(0.79,0.1),         # Place the legend at the bottom
    legend.spacing.y = unit(0.1,"cm"),
    legend.box = "vertical",          # Arrange legends horizontally
    legend.box.just = "left"          # Center the legend box
  ) 


ggsave(plot = model_performance_all, filename = paste0(pdf_directory,"/model_performance_all.pdf"),
       width = 7.2, height = 5.76)
ggsave(plot = model_performance_all, filename = paste0(png_directory,"/model_performance_all.png"),
       width = 7.2, height = 5.76)


datasets = c("plasmid", "four_kingdom", "splice_sites_donors",
             "promoter_no_tata", "H2AFZ")
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
    
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    legend.position = c(0.65,0.11),         # Place the legend at the bottom
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
  

ggsave(plot = model_performance_subset, filename = paste0(pdf_directory,"/model_performance_all_subset.pdf"),
       width = 4.3, height = 2.8)
ggsave(plot = model_performance_subset, filename = paste0(png_directory,"/model_performance_all_subset.png"),
       width = 4.3, height = 2.8)


vocab_4 = ggplot(
  df_sum %>%
    filter(metric == "eval_mcc", 
           vocab_size == 4096),
  aes(
    x = model_info,
    y = avg_score,
    color = model_type,
    shape = evaluation_set
  )
) +
  geom_point(alpha = 0.5) +
  facet_wrap( ~ dataset, scales = "free_y") +
  geom_point(alpha = 0.5) +
  scale_shape_manual(values=c(17, 20)) +
  
  #scale_x_discrete(labels = vocab_size) +  # Ensure only specific vocab sizes are used
  labs(
    x = "Model Size (M)",
    y = "MCC",
    color = "Model Type",
    shape = "Evaluation Set"
  ) +
  guides(
    shape = guide_legend(order = 1, ncol = 1),    # Place shape legend second
    color = guide_legend(order = 2, ncol = 1) # Place color legend third in one row
  )+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top") +
  scale_color_manual(values = c("#0974c0", "#ecc21d", "#cd534d"))+
  omicsArt::theme_omicsEye()+
  theme(
    legend.position = c(0.8,0.1),         # Place the legend at the bottom
    legend.spacing.y = unit(0.1,"cm"),
    legend.box = "horizental",          # Arrange legends horizontally
    legend.box.just = "left"          # Center the legend box
  ) 

ggsave(plot = vocab_4, filename = paste0(pdf_directory,"/vocab_4.pdf"),
       width = 7.2, height = 5.76)
ggsave(plot = vocab_4, filename = paste0(png_directory,"/vocab_4.png"),
       width = 7.2, height = 5.76)


cl_models = c(
  "nt_50_avg", "nt_50_max", "nt_50_conv",
  "nucleotide-transformer-v2-50m-multi-species"
)
df_cl = all_data %>% 
  filter(model %in% cl_models,
         metric == "eval_mcc") 
df_cl$model_n = ifelse(df_cl$model=="nt_50_conv", "Concat",
                     ifelse(df_cl$model=="nt_50_max", "Max",
                     ifelse(df_cl$model=="nt_50_avg", "Mean",
                     "[CLS]")
                     )
)
df_cl$model_n = factor(df_cl$model_n, levels = c("[CLS]", "Mean",
                                                 "Max", "Concat")
                       )

p <- ggboxplot(df_cl %>% 
                 filter(metric == "eval_mcc",
                        evaluation_set == "validation"), x = "model_n", y = "score",
               color = "model_n", palette = "jco",
               add = "jitter", facet.by = "dataset",
               scales = "free_y", 
               add.params = list(size = 0.5, alpha = 0.5))+
  geom_point(
    data = df_cl %>%
      filter(metric == "eval_mcc", evaluation_set == "test"),
    aes(x = model_n, y = score, color = model_n),
    # color = "red",       # Customize point color
    size = 2,            # Customize point size
    shape = 17           # Customize point shape (e.g., triangle)
  )+
  scale_color_manual(values = c("#309344", "#1871c2", "#f08c02", "#9c36b5"))+
  omicsArt::theme_omicsEye()+
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        legend.position = c(.82,0.15),
        axis.ticks.x=element_blank())+
  guides(color = guide_legend(nrow = 4)) +
  labs(y = "MCC",
       color = "Model Type")

ggsave(plot = p, filename = paste0(pdf_directory,"/cl_heads.pdf"),
       width = 7.2, height = 5.76)  
ggsave(plot = p, filename = paste0(png_directory,"/cl_heads.png"),
       width = 7.2, height = 5.76)  

# time cl
df_cl = all_data %>% 
  filter(model %in% cl_models,
         metric == "eval_runtime") 
df_cl$model_n = ifelse(df_cl$model=="nt_50_conv", "Concat",
                       ifelse(df_cl$model=="nt_50_max", "Max",
                              ifelse(df_cl$model=="nt_50_avg", "Mean",
                                     "[CLS]")
                       )
)
df_cl$model_n = factor(df_cl$model_n, levels = c("[CLS]", "Mean",
                                                 "Max", "Concat")
)


p <- ggboxplot(df_cl %>% 
                 filter(metric == "eval_runtime",
                        evaluation_set == "validation"), x = "model_n", y = "score",
               color = "model_n", palette = "jco",
               add = "jitter", facet.by = "dataset",
               scales = "free_y", 
               add.params = list(size = 0.5, alpha = 0.5))+
  geom_point(
    data = df_cl %>%
      filter(metric == "eval_runtime", evaluation_set == "test"),
    aes(x = model_n, y = score, color = model_n),
    # color = "red",       # Customize point color
    size = 2,            # Customize point size
    shape = 17           # Customize point shape (e.g., triangle)
  )+
  omicsArt::theme_omicsEye()+
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        legend.position = c(.82,0.15),
        axis.ticks.x=element_blank())+
  guides(color = guide_legend(nrow = 4)) +
  labs(y = "Evaluation runtime",
       color = "Model Type")

ggsave(plot = p, filename = "cl_heads_runtime.pdf",
       width = 7.2, height = 5.76)  


# cpt
cpt_models = c(
  "seqsight_4096_512_46M",
  "seqsight_4096_512_46M-Me",
  "seqsight_4096_512_46M-Ms",
  "seqsight_4096_512_46M-Mp"
)
df_cpt = all_data %>% 
  filter(model %in% cpt_models,
         metric == "eval_mcc") 
df_cpt$model = factor(df_cpt$model, levels = c("seqsight_4096_512_46M-Me",
                                               "seqsight_4096_512_46M-Mp",
                                               "seqsight_4096_512_46M-Ms",
                                               "seqsight_4096_512_46M"))

p <- ggboxplot(df_cpt %>% 
                 filter(metric == "eval_mcc",
                        evaluation_set == "validation"), x = "model", y = "score",
               color = "model", palette = "jco",
               add = "jitter", facet.by = "dataset",
               scales = "free_y", 
               add.params = list(size = 0.5, alpha = 0.5))+
  geom_point(
    data = df_cpt %>%
      filter(metric == "eval_mcc", evaluation_set == "test"),
    aes(x = model, y = score, color = model),
    # color = "red",       # Customize point color
    size = 2,            # Customize point size
    shape = 17           # Customize point shape (e.g., triangle)
  )+
  scale_color_manual(values = c("#2e2a2b", "#cf4e9c", "#8b57a2", "#358db9"))+
  omicsArt::theme_omicsEye()+
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        legend.position = c(.82,0.15),
        axis.ticks.x=element_blank())+
  guides(color = guide_legend(nrow = 4)) +
  labs(y = "MCC",
       color = "Model Type")

ggsave(plot = p, filename = paste0(pdf_directory,"/continual_pretraining.pdf"),
       width = 7.2, height = 5.76)  
ggsave(plot = p, filename = paste0(png_directory,"/continual_pretraining.png"),
       width = 7.2, height = 5.76)  


# deberta architecture
deb_models = c(
  "seqsight_4096_512_15M",
  "seqsight_4096_512_23M-at-xsmall" ,
  "seqsight_4096_512_27M",
  "seqsight_4096_512_46M",
  "seqsight_4096_512_47M-at-small",
  "seqsight_4096_512_89M" ,
  "seqsight_4096_512_89M-at-base"
)
df_deb = all_data %>% 
  filter(model %in% deb_models,
         metric == "eval_mcc") 

df_deb$model = factor(df_deb$model, levels = deb_models)
p <- ggboxplot(df_deb %>% 
                 filter(metric == "eval_mcc",
                        evaluation_set == "validation"), x = "model", y = "score",
               color = "model", palette = "jco",
               add = "jitter", facet.by = "dataset",
               scales = "free_y", 
               add.params = list(size = 0.5, alpha = 0.5))+
  geom_point(
    data = df_deb %>%
      filter(metric == "eval_mcc", evaluation_set == "test"),
    aes(x = model, y = score, color = model),
    # color = "red",       # Customize point color
    size = 2,            # Customize point size
    shape = 17           # Customize point shape (e.g., triangle)
  )+
  omicsArt::theme_omicsEye()+
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        legend.position = c(.8,0.15),
        axis.ticks.x=element_blank())+
  guides(color = guide_legend(nrow = 7)) +
  labs(y = "MCC",
       color = "Model Type")

ggsave(plot = p, filename = paste0(pdf_directory,"/4096_deberta.pdf"),
       width = 7.2, height = 5.76)
ggsave(plot = p, filename = paste0(png_directory,"/4096_deberta.png"),
       width = 7.2, height = 5.76)


# ESM VS NT

nt_models = c(
  "seqsight_esm_4096_512_55M",
  "seqsight_esm_4096_512_97M",
  "nucleotide-transformer-v2-100m-multi-species",
  "nucleotide-transformer-v2-50m-multi-species"
)

df_nt = all_data %>% 
  filter(model %in% nt_models,
         metric == "eval_mcc") 


df_nt$model = ifelse(df_nt$model == "nucleotide-transformer-v2-100m-multi-species", "NT-100",
                     ifelse(df_nt$model == "nucleotide-transformer-v2-50m-multi-species", "NT-50",
                            df_nt$model))
df_nt$model = factor(df_nt$model, 
                     levels = c(
                       "seqsight_esm_4096_512_55M",
                       "NT-50",
                       "seqsight_esm_4096_512_97M",
                       "NT-100"
                       )
                       )
p <- ggboxplot(df_nt %>% 
                 filter(metric == "eval_mcc",
                        evaluation_set == "validation"), 
               x = "model", y = "score",
               color = "model", palette = "jco",
               add = "jitter", facet.by = "dataset",
               scales = "free_y", 
               add.params = list(size = 0.5, alpha = 0.5))+
  geom_point(
    data = df_nt %>%
      filter(metric == "eval_mcc", evaluation_set == "test"),
    aes(x = model, y = score, color = model),
    # color = "red",       # Customize point color
    size = 2,            # Customize point size
    shape = 17           # Customize point shape (e.g., triangle)
  )+
  scale_color_manual(values = c("#F6E08A", "#E6A29E", "#ecc21d", "#cd534d"))+
  omicsArt::theme_omicsEye()+
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        legend.position = c(.8,0.13),
        axis.ticks.x=element_blank())+
  guides(color = guide_legend(nrow = 7)) +
  labs(y = "MCC",
       color = "Model Type")

ggsave(plot = p, filename = paste0(pdf_directory,"/4096_ESM.pdf"),
       width = 7.2, height = 5.76)
ggsave(plot = p, filename = paste0(png_directory,"/4096_ESM.png"),
       width = 7.2, height = 5.76)


##seq models
seq_models = all_data %>% 
  filter(grepl("seqsight", model),
         !grepl("esm", model))
# find for esm in the model column
seq_models$model_type = "DeBERTa"

seq_models$model_info = NA

seq_models$model_info = unlist(strsplit(seq_models$model, "_"))[c(F,F,F,T)]

seq_models$vocab_size = NA
seq_models$vocab_size = unlist(strsplit(seq_models$model, "_"))[c(F,T,F,F)]

seq_models$model_info <- stringr::str_extract(seq_models$model_info, "^\\d{2,3}(?=M)")


seq_models$model_info = as.numeric(seq_models$model_info)
seq_models$vocab_size = as.numeric(seq_models$vocab_size)
seq_models$model_type = as.factor(seq_models$model_type)
seq_models_simple <- seq_models %>% filter(!grepl("at|M-M", model)) %>% 
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
sm_models = c(
  "seqsight_4096_512_15M",
  "seqsight_8192_512_17M" ,
  "seqsight_16384_512_22M",
  "seqsight_32768_512_30M",
  "seqsight_65536_512_47M"
)

for(i in 1:3){
  tmp = seq_models_simple %>% filter(rank==i, metric == "eval_mcc")
  level_df = tmp %>% group_by(model, vocab_size) %>% 
    summarise(n = n()) %>% 
    select(model, vocab_size) %>% 
    arrange(vocab_size)
  tmp$model <- factor(tmp$model, levels = level_df$model[order(level_df$vocab_size)])

p <- ggboxplot(tmp %>% 
                 filter(metric == "eval_mcc",
                        evaluation_set == "validation"), x = "model", y = "score",
               color = "model", palette = "jco",
               add = "jitter", facet.by = "dataset",
               scales = "free_y", 
               add.params = list(size = 0.5, alpha = 0.5))+
  geom_point(
    data = tmp %>%
      filter(metric == "eval_mcc", evaluation_set == "test"),
    aes(x = model, y = score, color = model),
    # color = "red",       # Customize point color
    size = 2,            # Customize point size
    shape = 17           # Customize point shape (e.g., triangle)
  )+
  omicsArt::theme_omicsEye()+
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        legend.position = c(.8,0.15),
        axis.ticks.x=element_blank())+
  guides(color = guide_legend(nrow = 7)) +
  labs(y = "MCC",
       color = "Model Type")

ggsave(plot = p, filename = paste0(pdf_directory, "/deberta_simple_",i,  ".pdf"),
       width = 7.2, height = 5.76)
ggsave(plot = p, filename = paste0(png_directory, "/deberta_simple_",i,  ".png"),
       width = 7.2, height = 5.76)

}
 
# top selected

tp_models = c(
  "seqsight_esm_4096_512_97M",
  "nucleotide-transformer-v2-100m-multi-species",
  "seqsight_4096_512_89M-at-base",
  "seqsight_4096_512_89M-at-base-multi"
)

df_tp = all_data %>% 
  filter(model %in% tp_models,
         metric == "eval_mcc") 

df_tp$model = ifelse(df_tp$model == "nucleotide-transformer-v2-100m-multi-species", "NT-100",
                     df_tp$model)
df_tp$model = factor(df_tp$model, 
                     levels = c(
                       "seqsight_4096_512_89M-at-base-multi",
                       "seqsight_4096_512_89M-at-base",
                       "seqsight_esm_4096_512_97M",
                       "NT-100"
                     )
)
p <- ggboxplot(df_tp %>% 
                 filter(metric == "eval_mcc",
                        evaluation_set == "validation"), 
               x = "model", y = "score",
               color = "model", palette = "jco",
               add = "jitter", facet.by = "dataset",
               scales = "free_y", 
               add.params = list(size = 0.5, alpha = 0.5))+
  geom_point(
    data = df_tp %>%
      filter(metric == "eval_mcc", evaluation_set == "test"),
    aes(x = model, y = score, color = model),
    # color = "red",       # Customize point color
    size = 2,            # Customize point size
    shape = 17           # Customize point shape (e.g., triangle)
  )+
  scale_color_manual(values = c("#05395e", "#0974c0", "#ecc21d", "#cd534d"))+
  omicsArt::theme_omicsEye()+
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        legend.position = c(.77,0.08),
        axis.ticks.x=element_blank())+
  guides(color = guide_legend(nrow = 7)) +
  labs(y = "MCC",
       color = "Model Type")

ggsave(plot = p, filename = paste0(pdf_directory, "/top_models.pdf"),
       width = 7.2, height = 5.76)
ggsave(plot = p, filename = paste0(png_directory, "/top_models.png"),
       width = 7.2, height = 5.76)




# esm vs deberta

tp_models = c(
  "seqsight_4096_512_47M-at-small",
  "seqsight_esm_4096_512_55M",
  "seqsight_4096_512_89M-at-base",
  "seqsight_esm_4096_512_97M"
  
)

df_tp = all_data %>% 
  filter(model %in% tp_models,
         metric == "eval_mcc") 

df_tp$model = factor(df_tp$model, 
                     levels = tp_models)
p <- ggboxplot(df_tp %>% 
                 filter(metric == "eval_mcc",
                        evaluation_set == "validation"), 
               x = "model", y = "score",
               color = "model", palette = "jco",
               add = "jitter", facet.by = "dataset",
               scales = "free_y", 
               add.params = list(size = 0.5, alpha = 0.5))+
  geom_point(
    data = df_tp %>%
      filter(metric == "eval_mcc", evaluation_set == "test"),
    aes(x = model, y = score, color = model),
    # color = "red",       # Customize point color
    size = 2,            # Customize point size
    shape = 17           # Customize point shape (e.g., triangle)
  )+
  scale_color_manual(values = c("#75B3DF", "#F6E08A", "#0974c0","#ecc21d"))+
  omicsArt::theme_omicsEye()+
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        legend.position = c(.8,0.13),
        axis.ticks.x=element_blank())+
  guides(color = guide_legend(nrow = 7)) +
  labs(y = "MCC",
       color = "Model Type")

ggsave(plot = p, filename = paste0(pdf_directory, "/esm_vs_deberta.pdf"),
       width = 7.2, height = 5.76)
ggsave(plot = p, filename = paste0(png_directory, "/esm_vs_deberta.png"),
       width = 7.2, height = 5.76)

# nt vs esm vs deberta
tp_models = c(
  "seqsight_4096_512_47M-at-small",
  "seqsight_esm_4096_512_55M",
  "seqsight_4096_512_89M-at-base",
  "seqsight_4096_512_89M-at-base-multi",
  "seqsight_esm_4096_512_97M",
  "nucleotide-transformer-v2-100m-multi-species",
  "nucleotide-transformer-v2-50m-multi-species"
  
)

df_tp = all_data %>% 
  filter(model %in% tp_models,
         metric == "eval_mcc") 

df_tp$model = ifelse(df_tp$model == "nucleotide-transformer-v2-100m-multi-species", "NT-100",
                     ifelse(df_tp$model == "nucleotide-transformer-v2-50m-multi-species", "NT-50",
                            df_tp$model))
df_tp$model = factor(df_tp$model, 
                     levels = c(
                       "seqsight_esm_4096_512_55M",
                       "NT-50",
                       "seqsight_4096_512_47M-at-small",
                       "seqsight_esm_4096_512_97M",
                       "NT-100",
                       "seqsight_4096_512_89M-at-base",
                       "seqsight_4096_512_89M-at-base-multi"
                     )
)


p <- ggboxplot(df_tp %>% 
                 filter(metric == "eval_mcc",
                        evaluation_set == "validation"), 
               x = "model", y = "score",
               color = "model", palette = "jco",
               add = "jitter", facet.by = "dataset",
               scales = "free_y", 
               add.params = list(size = 0.5, alpha = 0.5))+
  geom_point(
    data = df_tp %>%
      filter(metric == "eval_mcc", evaluation_set == "test"),
    aes(x = model, y = score, color = model),
    # color = "red",       # Customize point color
    size = 2,            # Customize point size
    shape = 17           # Customize point shape (e.g., triangle)
  )+
  scale_color_manual(values = c("#F6E08A", "#E6A29E", "#75B3DF", "#ecc21d", "#cd534d", "#0974c0","#05395e"))+
  omicsArt::theme_omicsEye()+
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        legend.position = c(.78,0.13),
        axis.ticks.x=element_blank())+
  guides(color = guide_legend(nrow = 7)) +
  labs(y = "MCC",
       color = "Model Type")

ggsave(plot = p, filename = paste0(pdf_directory, "/nt_esm_vs_deberta.pdf"),
       width = 7.2, height = 5.76)
ggsave(plot = p, filename = paste0(png_directory, "/nt_esm_vs_deberta.png"),
       width = 7.2, height = 5.76)



