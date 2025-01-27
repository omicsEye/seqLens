library(tidyverse)
library(ggpubr)
library(ggsci)
library(cowplot)

plasmid = read.csv("plasmid_umap.csv")
four_kingdom = read.csv("four_kingdoms_umap.csv")


plasmid <- plasmid %>%
  mutate(facet_Label = paste(fine_tuned, vec_type, sep = " - "))


four_kingdom <- four_kingdom %>%
  mutate(facet_Label = paste(fine_tuned, vec_type, sep = " - "))

plasmid$fine_tuned = ifelse(plasmid$fine_tuned == "Yes",
                            "Fine-tuned", "Base model")

four_kingdom$fine_tuned = ifelse(four_kingdom$fine_tuned == "Yes",
                            "Fine-tuned", "Base model")
p1 = ggplot(plasmid, aes(x = UMAP1, y = UMAP2, color = Label))+
  geom_point(alpha = 0.2, size = 0.2)+
  facet_grid(fine_tuned ~ vec_type, scales = "free_y")+ 
  scale_color_jco()+
  omicsArt::theme_omicsEye()+
  theme(
    panel.spacing = unit(0.5, "lines"),
    strip.background = element_rect(fill = "gray", color = "black"),  # Change strip background color
    strip.text = element_text(size = 6, face = "bold", color = "Black"),  # Customize text
    strip.placement = "outside", # Position the facet labels outside
    legend.position = c(0.9, 0.6),
    legend.title = element_blank()
  )+
  guides(color = guide_legend(override.aes = list(alpha = 1, size = 1)))


p2 = ggplot(four_kingdom, aes(x = UMAP1, y = UMAP2, color = Label))+
  geom_point(alpha = 0.2, size = 0.2)+
  facet_grid(fine_tuned ~ vec_type, scales = "free_y")+ 
  scale_color_jco()+
  omicsArt::theme_omicsEye()+
  theme(
    panel.spacing = unit(0.5, "lines"),
    strip.background = element_rect(fill = "gray", color = "black"),  # Change strip background color
    strip.text = element_text(size = 6, face = "bold", color = "Black"),  # Customize text
    strip.placement = "outside", # Position the facet labels outside
    legend.position = c(0.9, 0.6),
    legend.title = element_blank()
    )+
   guides(color = guide_legend(override.aes = list(alpha = 1, size = 1)))


ggsave(plot = p1, filename = "plasmid.pdf",
       width = 7.2, height = 5)


ggsave(plot = p2, filename = "four_kingdom.pdf",
       width = 7.2, height = 5)


p3 = ggplot(plasmid, aes(x = UMAP1, y = UMAP2, color = Label)) +
  geom_point(alpha = 0.2, size = 0.2) +
  facet_wrap(.~facet_Label, scales = "free") + 
  scale_color_jco() +
  omicsArt::theme_omicsEye() +
  theme(
    panel.spacing = unit(0.5, "lines"),
    strip.background = element_rect(fill = "gray", color = "black"),  # Change strip background color
    strip.text = element_text(size = 8, face = "bold", color = "Black"),  # Customize text
    strip.placement = "outside",  # Position the facet labels outside
    legend.position = c(0.87, 0.1),
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 10)
  ) +
  guides(color = guide_legend(override.aes = list(alpha = 1, size = 1)))

p4 = ggplot(four_kingdom, aes(x = UMAP1, y = UMAP2, color = Label)) +
  geom_point(alpha = 0.2, size = 0.2) +
  facet_wrap(.~facet_Label, scales = "free") + 
  scale_color_jco() +
  omicsArt::theme_omicsEye() +
  theme(
    panel.spacing = unit(0.5, "lines"),
    strip.background = element_rect(fill = "gray", color = "black"),  # Change strip background color
    strip.text = element_text(size = 8, face = "bold", color = "Black"),  # Customize text
    strip.placement = "outside",  # Position the facet labels outside
    legend.position = c(0.9, 0.1),
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 10)
  ) +
  guides(color = guide_legend(override.aes = list(alpha = 1, size = 1)))

ggsave(plot = p3, filename = "plasmid_facet.pdf",
       width = 7.2, height = 5)


ggsave(plot = p4, filename = "four_kingdom_facet.pdf",
       width = 7.2, height = 5)



####
# Required package
if (!require("aricode")) install.packages("aricode")

# Initialize categories and scores dataframe
categories = unique(four_kingdom$facet_Label)
clusterin_scores_four_kingdom = data.frame(categories)
ari = c()  # Adjusted Rand Index
ami = c()  # Adjusted Mutual Information
nid = c()  # Normalized information distance (NID)
nmi = c()  # Normalized Mutual Information (NMI)

# Loop through categories
for (category in categories) {
  # Filter data for the category
  tmp = four_kingdom %>%
    filter(facet_Label == category) %>%
    select(UMAP1, UMAP2, Label)
  
  # Perform k-means clustering
  kmeans_res <- kmeans(tmp[, 1:2], centers = length(unique(tmp$Label)))
  
  # Evaluate clustering performance
  measure = aricode::clustComp(kmeans_res$cluster, tmp$Label)
  
  # Store metrics
  ari = c(ari, measure$ARI)  # Adjusted Rand Index
  ami = c(ami, measure$AMI)  # Adjusted Mutual Information
  nid = c(nid, measure$AMI)  # Normalized information distance (NID)
  nmi = c(nmi, measure$NMI)  # Normalized Mutual Information
}

# Add scores to the dataframe
clusterin_scores_four_kingdom$ARI = ari
clusterin_scores_four_kingdom$AMI = ami
clusterin_scores_four_kingdom$NID = nid
clusterin_scores_four_kingdom$NMI = nmi

categories = unique(plasmid$facet_Label)
clusterin_scores_plasmid = data.frame(categories)
ari = c()  # Adjusted Rand Index
ami = c()  # Adjusted Mutual Information
nid = c()  # Normalized information distance (NID)
nmi = c()  # Normalized Mutual Information (NMI)

# Loop through categories
for (category in categories) {
  # Filter data for the category
  tmp = plasmid %>%
    filter(facet_Label == category) %>%
    select(UMAP1, UMAP2, Label)
  
  # Perform k-means clustering
  kmeans_res <- kmeans(tmp[, 1:2], 
                       centers = length(unique(tmp$Label)))
  
  # Evaluate clustering performance
  measure = aricode::clustComp(kmeans_res$cluster,
                               tmp$Label)
  
  # Store metrics
  ari = c(ari, measure$ARI)  # Adjusted Rand Index
  ami = c(ami, measure$AMI)  # Adjusted Mutual Information
  nid = c(nid, measure$AMI)  # Normalized information distance (NID)
  nmi = c(nmi, measure$NMI)  # Normalized Mutual Information
}

# Add scores to the dataframe
clusterin_scores_plasmid$ARI = ari
clusterin_scores_plasmid$AMI = ami
clusterin_scores_plasmid$NID = nid
clusterin_scores_plasmid$NMI = nmi



df_table = clusterin_scores_four_kingdom %>% 
  gather("metric", "values", -categories) %>% 
  mutate(dataset = "Four kingdom") %>% 
  bind_rows(
    clusterin_scores_plasmid %>% 
      gather("metric", "values", -categories) %>% 
      mutate(dataset = "Plasmid")
  ) %>% 
  mutate(values = ifelse(values<0, 0, values))


# Heatmap plot
heatmap = ggplot(df_table, aes(x = categories, y = metric, fill = values)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%.3f", values)), 
            size = 2, color = "black") +
  scale_fill_gradient(low = "white", high = "steelblue", name = "Score") +
  labs(
    y = "Metrics"
  ) +
  facet_wrap(.~dataset)+
  omicsArt::theme_omicsEye()+
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_blank()
  )

ggsave(plot = heatmap, filename = "heatmap_table.pdf",
       width = 7.2, height = 1.5)



# pair pca plots
pca_plots = list()
list_files = list.files("vector_data/")
for(file_name in list_files){
  print(file_name)
  
  df = read.csv(paste0("vector_data/", file_name), row.names = 1)
  vec_types = unique(df$vec_type)
  for(vec_type_f in vec_types){
    tmp = df %>% 
      filter(vec_type==vec_type_f) %>% 
      select(-vec_type)
    tmp_label = tmp$label
    model_type = tmp$model_type
    tmp = tmp %>% select(-label, -model_type)
    
    pca_dat = prcomp(as.matrix(tmp), center = TRUE, scale. = TRUE)
    tmp2 = data.frame(pca1 = pca_dat$x[,1],
                      pca2 = pca_dat$x[,2],
                      label = tmp_label
    )
    
    plot_title = paste0(model_type[1], " - ", vec_type_f)
    if(grepl("plasmid", file_name)){
      plot_name = paste("plasmid", plot_title, sep = "_")
    }else{
      plot_name = paste("four_kingdom", plot_title, sep = "_")
    }
    pca_plots[[plot_name]] = ggplot(tmp2, 
                                    aes(x = pca1, y = pca2, color = label)) +
      geom_point(alpha = 0.2, size = 0.2) +
      scale_color_jco() +
      labs(x = paste0("PC1 (", round(pca_dat$sdev[1],1), "%)"),
           y = paste0("PC2 (", round(pca_dat$sdev[2],1), "%)"),
           title = plot_title)+
      omicsArt::theme_omicsEye() +
      theme(
        panel.spacing = unit(0.5, "lines"),
        # legend.position = c(0.9, 0.1),
        # legend.title = element_blank(),
        # legend.text = element_text(size = 8),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 8)
      ) +
      guides(color = guide_legend(override.aes = list(alpha = 1, size = 1)))
    
  }
  
  
}



fig1 = ggdraw() +
  draw_plot(pca_plots[["four_kingdom_Base - CLS"]] + theme(legend.position = "none"),
            x = 0.0, y = 0.5, width = 0.3, height = .5)+
  draw_plot(pca_plots[["four_kingdom_Base - Mean"]] + theme(legend.position = "none"),
            x = 0.33, y = 0.5, width = 0.3, height = .5)+
  draw_plot(pca_plots[["four_kingdom_Base - Max"]] + theme(legend.position = "none"),
            x = 0.66, y = 0.5, width = 0.3, height = .5)+
  draw_plot(pca_plots[["four_kingdom_Fine-tuned - CLS"]] + theme(legend.position = "none"),
            x = 0.0, y = 0.0, width = 0.3, height = .5)+
  draw_plot(pca_plots[["four_kingdom_Fine-tuned - Mean"]] + theme(legend.position = "none"),
            x = 0.33, y = 0.0, width = 0.3, height = .5)+
  draw_plot(pca_plots[["four_kingdom_Fine-tuned - Max"]] + theme(legend.position = "none") ,
            x = 0.66, y = 0.0, width = 0.3, height = .5) +
  draw_plot_label((label = c("a")),
                  size = 8,x = c(0),
                  y = c(1))

fig2 = ggdraw() +
  draw_plot(pca_plots[["plasmid_Base - CLS"]] + theme(legend.position = "none"),
            x = 0.0, y = 0.5, width = 0.3, height = .5)+
  draw_plot(pca_plots[["plasmid_Base - Mean"]] + theme(legend.position = "none"),
            x = 0.33, y = 0.5, width = 0.3, height = .5)+
  draw_plot(pca_plots[["plasmid_Base - Max"]] + theme(legend.position = "none"),
            x = 0.66, y = 0.5, width = 0.3, height = .5)+
  draw_plot(pca_plots[["plasmid_Fine-tuned - CLS"]] + theme(legend.position = "none"),
            x = 0.0, y = 0.0, width = 0.3, height = .5)+
  draw_plot(pca_plots[["plasmid_Fine-tuned - Mean"]] + theme(legend.position = "none"),
            x = 0.33, y = 0.0, width = 0.3, height = .5)+
  draw_plot(pca_plots[["plasmid_Fine-tuned - Max"]] + theme(legend.position = "none") ,
            x = 0.66, y = 0.0, width = 0.3, height = .5) +
  draw_plot_label((label = c("b")),
                  size = 8,x = c(0),
                  y = c(1))
legend_fourkingdom = get_legend(pca_plots[["four_kingdom_Base - CLS"]])
legend_plasmid = get_legend(pca_plots[["plasmid_Base - CLS"]])

fig_comb = ggdraw() +
  draw_plot(fig1,
            x = 0.0, y = 0.5, width = 1, height = .5)+
  draw_plot(legend_fourkingdom,
            x = 0.9, y = 0.65, width = 0.1, height = .1)+
  draw_plot(fig2,
            x = 0.0, y = 0.0, width = 1, height = .5)+
  draw_plot(legend_plasmid,
            x = 0.9, y = 0.15, width = 0.1, height = .1)


ggsave(plot = fig_comb, filename = "pca_plots.pdf",
       width = 7.2, height = 7.2)
