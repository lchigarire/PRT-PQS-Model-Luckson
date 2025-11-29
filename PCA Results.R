library(dplyr)
library(FactoMineR)
library(factoextra)

num_df <- df2 %>%
  dplyr::select(where(is.numeric))

str(num_df)

pca_model <- PCA(num_df, scale.unit = TRUE, graph = FALSE)

#Scree Plot_Variance Explained
p_scree <- fviz_eig(pca_model, addlabels = TRUE, ylim = c(0, 50))
p_scree

ggplot2::ggsave("pca_scree_df2.png", plot = p_scree,
                width = 6, height = 4, dpi = 300)

p_biplot <- fviz_pca_biplot(
  pca_model,
  geom.ind = "point",
  pointshape = 21,
  pointsize = 1,
  alpha.ind = 0.4,
  col.var = "red",
  repel = TRUE
)

p_biplot

ggplot2::ggsave("pca_biplot_df2.png", plot = p_biplot,
                width = 7, height = 6, dpi = 300)

cluster_factor <- as.factor(df2$Cluster)

p_ind_cluster <- fviz_pca_ind(
  pca_model,
  geom = "point",
  habillage = cluster_factor,
  addEllipses = TRUE,
  pointshape = 21,
  pointsize = 1.2,
  alpha = 0.6
)

p_ind_cluster

ggplot2::ggsave("pca_individuals_by_cluster_df2.png", plot = p_ind_cluster,
                width = 6, height = 5, dpi = 300)

pca_model$ind$coord

library(plotly)

scores <- as.data.frame(pca_model$ind$coord)

scores$Cluster <- as.factor(df2$Cluster)

scores$PC1color <- scales::rescale(scores$Dim.1, to = c(0, 1))

fig2 <- plot_ly(
  scores,
  x = ~Dim.1,
  y = ~Dim.2,
  z = ~Dim.3,
  color = ~PC1color,
  colors = colorRamp(c("blue", "green", "yellow", "red")),
  type = "scatter3d",
  mode = "markers",
  marker = list(size = 2, opacity = 0.7)
)

fig2


