num_df    <- df2 |> dplyr::select(where(is.numeric))
pca_model <- FactoMineR::PCA(num_df, scale.unit = TRUE, graph = FALSE)

library(dplyr)
library(FactoMineR)
library(factoextra)
library(ggplot2)

scores <- as.data.frame(pca_model$ind$coord)   # Dim.1, Dim.2, Dim.3, ...

set.seed(42)

X <- scores[, 1:3]

km_res <- kmeans(X, centers = 5, nstart = 25)

df2$Cluster    <- factor(km_res$cluster)
scores$Cluster <- df2$Cluster

p_cluster <- fviz_cluster(
  km_res,
  data  = X,
  geom  = "point",
  ellipse.type = "norm",
  show.clust.cent = TRUE,
  repel = TRUE
)

p_cluster
ggsave("cluster_scatter_pc1_pc2.png", p_cluster, width = 7, height = 4.5, dpi = 300)


library(cluster)

sil <- silhouette(km_res$cluster, dist(X))

p_sil <- fviz_silhouette(sil)
p_sil
ggsave("cluster_silhouette_k5.png", p_sil, width = 7, height = 4.5, dpi = 300)


# Distance on the same PCA space
d_mat <- dist(X)

hc_res <- hclust(d_mat, method = "ward.D2")

p_dend <- fviz_dend(
  hc_res,
  k = 5,                
  rect = TRUE,
  show_labels = FALSE
)

p_dend
ggsave("hierarchical_dendrogram_ward.png", p_dend, width = 7, height = 4.5, dpi = 300)


library(scales)
library(fmsb)   

vars <- c("Age", "BenefitScaled", "ReserveScaled",
          "LongevityFactor", "HealthGroup", "EnvScore", "FundingRatio")

cluster_summary <- df2 |>
  group_by(Cluster) |>
  summarise(across(all_of(vars), mean, na.rm = TRUE)) |>
  as.data.frame()

cluster_summary

cluster_scaled <- cluster_summary
cluster_scaled[ , vars] <- lapply(cluster_summary[ , vars], rescale)

radar_data <- cluster_scaled[ , vars]

# Add max and min rows
max_row <- apply(radar_data, 2, max)
min_row <- apply(radar_data, 2, min)

radar_data <- rbind(max_row, min_row, radar_data)
rownames(radar_data) <- c("MAX", "MIN", paste0("Cluster_", cluster_summary$Cluster))
radar_data

png("cluster_profiles_radar.png", width = 800, height = 600, res = 150)

radarchart(
  radar_data,
  axistype = 1,
  pcol = c("red", "gold", "forestgreen", "deepskyblue", "purple"),
  plwd = 2,
  plty = 1,
  cglcol = "grey",
  cglty = 1,
  cglwd = 0.8,
  axislabcol = "grey20",
  vlcex = 0.8
)
legend("topright",
       legend = paste("Cluster", cluster_summary$Cluster),
       col = c("red", "gold", "forestgreen", "deepskyblue", "purple"),
       lty = 1, lwd = 2, cex = 0.8)

dev.off()

str(df2$Cluster)

library(dplyr)
library(scales)
library(fmsb)

vars <- c("Age",
          "BenefitScaled",
          "ReserveScaled",
          "LongevityFactor",
          "HealthGroup",
          "EnvScore",
          "FundingRatio")

cluster_summary <- df2 %>%
  group_by(Cluster) %>%
  summarise(across(all_of(vars), mean, na.rm = TRUE)) %>%
  ungroup() %>%
  as.data.frame()

cluster_summary

cluster_scaled <- cluster_summary
cluster_scaled[ , vars] <- lapply(cluster_summary[ , vars], rescale)

radar_data <- cluster_scaled[ , vars]

max_row <- apply(radar_data, 2, max)
min_row <- apply(radar_data, 2, min)

radar_data <- rbind(max_row, min_row, radar_data)
rownames(radar_data) <- c("MAX", "MIN",
                          paste0("Cluster_", cluster_summary$Cluster))

radar_data


radarchart(
  radar_data,
  axistype   = 1,
  pcol       = c("red", "gold", "forestgreen", "deepskyblue", "purple"),
  plwd       = 2,
  plty       = 1,
  cglcol     = "grey70",
  cglty      = 1,
  cglwd      = 0.8,
  axislabcol = "grey20",
  vlcex      = 0.8
)
legend("topright",
       legend = paste("Cluster", cluster_summary$Cluster),
       col    = c("red", "gold", "forestgreen", "deepskyblue", "purple"),
       lty    = 1, lwd = 2, cex = 0.8)



