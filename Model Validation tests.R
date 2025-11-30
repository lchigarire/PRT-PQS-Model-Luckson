library(cluster)
library(factoextra)

# distance in PCA space
dist_mat <- dist(scores[, 1:3])

sil <- silhouette(df$Cluster, dist_mat)
summary(sil)

# Plot for thesis
p_sil <- fviz_silhouette(sil) +
  theme_minimal(base_size = 14) +
  labs(title = "Silhouette Plot for k = 5 Clusters")

p_sil

#ELBOW PLOT
set.seed(42)
k_values <- 2:8

wcss <- sapply(k_values, function(k){
  kmeans(scores[,1:3], centers = k, nstart = 25)$tot.withinss
})

elbow_df <- data.frame(k = k_values, WCSS = wcss)

p_elbow <- ggplot(elbow_df, aes(x = k, y = WCSS)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Elbow Plot for k-means Clustering",
    x = "Number of clusters (k)",
    y = "Total within-cluster sum of squares"
  ) +
  theme_minimal(base_size = 14)

p_elbow

#ADJUSTED RAND INDEX UNDER RESAMPLING
install.packages("mclust")
library(mclust)  # for adjustedRandIndex

set.seed(123)
B <- 30               # number of bootstrap runs
ARI <- numeric(B)

for (b in 1:B) {
  idx <- sample(1:nrow(scores), replace = TRUE)
  km_b <- kmeans(scores[idx, 1:3], centers = 5, nstart = 25)
  ARI[b] <- adjustedRandIndex(km_res$cluster[idx], km_b$cluster)
}

mean_ARI <- mean(ARI)
sd_ARI   <- sd(ARI)

mean_ARI
sd_ARI
p_ari <- ggplot(data.frame(ARI = ARI), aes(x = ARI)) +
  geom_histogram(bins = 10, color = "white") +
  theme_minimal(base_size = 14) +
  labs(title = "Bootstrap Cluster Stability (Adjusted Rand Index)")

p_ari

#jitter numeric inputs and rerun PCA
num_df <- df %>% select(where(is.numeric))

pca1 <- FactoMineR::PCA(num_df, scale.unit = TRUE, graph = FALSE)
load1 <- pca1$var$coord[, 1:3]  # loadings for PC1–PC3

set.seed(99)
num_df_jit <- num_df %>%
  mutate(across(everything(),
                ~ . + rnorm(n(), mean = 0, sd = sd(.) * 0.01)))

pca2 <- FactoMineR::PCA(num_df_jit, scale.unit = TRUE, graph = FALSE)
load2 <- pca2$var$coord[, 1:3]

# correlations of loadings for each PC
pc_cor <- sapply(1:3, function(j) cor(load1[, j], load2[, j]))
pc_cor

#PQS under random noise on key inputs
set.seed(777)
R_runs <- 50
PQS_noise <- numeric(R_runs)

for (r in 1:R_runs) {
  df_n <- df %>%
    mutate(
      Benefit_n = Benefit * (1 + rnorm(n(), 0, 0.05)),
      Reserve_n = Reserve * (1 + rnorm(n(), 0, 0.05))
    )
  
  # recompute weights and weighted risk
  df_n <- df_n %>%
    mutate(
      LiabWeight_n   = Reserve_n / sum(Reserve_n),
      RiskScore_n    = Deviation * LongevityFactor,  # structure unchanged
      WeightedRisk_n = RiskScore_n * LiabWeight_n
    )
  
  PQS_noise[r] <- sum(df_n$WeightedRisk_n) * (1 + ConcentrationPenalty)
}

mean(PQS_noise)
sd(PQS_noise)
sd(PQS_noise) / PQS_base * 100    # CV in percent

p_pqs_noise <- ggplot(data.frame(PQS = PQS_noise), aes(x = PQS)) +
  geom_histogram(bins = 15, color = "white") +
  geom_vline(xintercept = PQS_base, linetype = "dashed") +
  theme_minimal(base_size = 14) +
  labs(title = "PQS Robustness Under 5% Input Noise")

p_pqs_noise

#PQS UNDER RESAMPLING
set.seed(888)
R_runs <- 50
PQS_sub <- numeric(R_runs)

for (r in 1:R_runs) {
  idx <- sample(1:nrow(df), size = floor(0.8 * nrow(df)), replace = FALSE)
  df_s <- df[idx, ]
  
  df_s <- df_s %>%
    mutate(
      LiabWeight_s   = Reserve / sum(Reserve),
      WeightedRisk_s = Deviation * LongevityFactor * LiabWeight_s
    )
  
  PQS_sub[r] <- sum(df_s$WeightedRisk_s) * (1 + ConcentrationPenalty)
}

mean(PQS_sub)
sd(PQS_sub)
sd(PQS_sub) / PQS_base * 100

p_pqs_sub <- ggplot(data.frame(PQS = PQS_sub), aes(x = PQS)) +
  geom_histogram(bins = 15, color = "white") +
  geom_vline(xintercept = PQS_base, linetype = "dashed") +
  theme_minimal(base_size = 14) +
  labs(title = "PQS Robustness Under 80% Subsampling")

p_pqs_sub


