library(readr)
library(dplyr)
library(FactoMineR)
library(factoextra)
library(cluster)
library(ggplot2)

head(prt_quality_dataset_csv)

df<- prt_quality_dataset_csv

#PCA preparation
num_df <- df %>% select(where(is.numeric))

pca_model <- PCA(num_df, scale.unit = TRUE, graph = FALSE)
scores <- as.data.frame(pca_model$ind$coord)

#clustering
set.seed(42)
km_res <- kmeans(scores[,1:3], centers = 5, nstart = 50)

df$Cluster <- km_res$cluster
scores$Cluster <- km_res$cluster

#deviation scoring
centers <- km_res$centers

df$Deviation <- sapply(1:nrow(df), function(i){
  cl <- df$Cluster[i]
  sqrt(sum((scores[i,1:3] - centers[cl,])^2))
})

#RiskScore and PQS
df <- df %>% mutate(
  RiskScore = Deviation * LongevityFactor,
  LiabWeight = Reserve / sum(Reserve),
  WeightedRisk = RiskScore * LiabWeight
)

ConcentrationPenalty <- 0.05  # or your entropy-based value

PQS_base <- sum(df$WeightedRisk) * (1 + ConcentrationPenalty)
PQS_base

#Global Longevity Shock Sensitivity
longevity_shocks <- c(0, 0.01, 0.02, 0.03, 0.05)

sens_global <- lapply(longevity_shocks, function(sh){
  df_s <- df %>% mutate(
    LongevityFactor_s = LongevityFactor * (1 + sh),
    RiskScore_s       = Deviation * LongevityFactor_s,
    WeightedRisk_s    = RiskScore_s * LiabWeight
  )
  PQS_s <- sum(df_s$WeightedRisk_s) * (1 + ConcentrationPenalty)
  return(PQS_s)
}) %>% unlist()

sens_df_global <- data.frame(
  Shock = longevity_shocks * 100,
  PQS   = sens_global
)

# PLOT
p_global <- ggplot(sens_df_global, aes(x = Shock, y = PQS)) +
  geom_line(color = "firebrick", size = 1.2) +
  geom_point(color = "firebrick", size = 3) +
  geom_hline(yintercept = PQS_base, linetype = "dashed", color = "grey40") +
  labs(
    title = "Global Longevity Shock Sensitivity",
    x = "Longevity Improvement (%)",
    y = "PQS"
  ) +
  theme_minimal(base_size = 14)

p_global

#TRAGETED LONGEVITY SHOCK CODE

library(dplyr)

df <- df %>%
  mutate(
    LongevityGroup = case_when(
      LongevityGroup_5 == 1 ~ 5L,
      LongevityGroup_4 == 1 ~ 4L,
      LongevityGroup_3 == 1 ~ 3L,
      LongevityGroup_2 == 1 ~ 2L,
      TRUE                  ~ 1L   # all dummies zero → group 1
    )
  )

longevity_shocks <- c(0, 0.01, 0.02, 0.03, 0.05)

sens_target <- sapply(longevity_shocks, function(sh) {
  df_s <- df %>%
    mutate(
      LongevityFactor_s = ifelse(LongevityGroup >= 4,
                                 LongevityFactor * (1 + sh),
                                 LongevityFactor),
      RiskScore_s    = Deviation * LongevityFactor_s,
      WeightedRisk_s = RiskScore_s * LiabWeight
    )
  sum(df_s$WeightedRisk_s) * (1 + ConcentrationPenalty)
})

sens_df_target <- data.frame(
  Shock = longevity_shocks * 100,
  PQS   = sens_target
)

library(ggplot2)

p_target <- ggplot(sens_df_target, aes(x = Shock, y = PQS)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  geom_hline(yintercept = PQS_base, linetype = "dashed", color = "grey40") +
  labs(
    title = "Targeted Longevity Shock (Groups 4–5)",
    x = "Longevity Improvement on Groups 4–5 (%)",
    y = "PQS"
  ) +
  theme_minimal(base_size = 14)

p_target

target_increase_pct <- 100 * (
  sens_df_target$PQS[sens_df_target$Shock == 5] - PQS_base
) / PQS_base

target_increase_pct

#RESERVE CONCENTRATION SHOCK
# Simulate concentration shock (example: move 10% to Cluster 1)
df_res <- df
shift <- 0.10 * sum(df$Reserve)

df_res$Reserve[df$Cluster == 3][1] <- df_res$Reserve[df$Cluster == 3][1] - shift
df_res$Reserve[df$Cluster == 1][1] <- df_res$Reserve[df$Cluster == 1][1] + shift

df_res <- df_res %>% mutate(
  LiabWeight = Reserve / sum(Reserve),
  WeightedRisk = RiskScore * LiabWeight
)

PQS_shift <- sum(df_res$WeightedRisk) * (1 + ConcentrationPenalty)

sens_df_res <- data.frame(
  Scenario = c("Base PQS", "After Liability Shift"),
  PQS = c(PQS_base, PQS_shift)
)

# PLOT
p_reserve <- ggplot(sens_df_res, aes(x = Scenario, y = PQS, fill = Scenario)) +
  geom_col(width = 0.6, alpha = 0.8) +
  scale_fill_manual(values = c("steelblue", "firebrick")) +
  labs(
    title = "Reserve Concentration Shock Test",
    x = "",
    y = "PQS"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

p_reserve

#OUTLIER REMOVAL
cutoff <- quantile(df$Deviation, 0.99)

df_trim <- df %>% filter(Deviation <= cutoff)
PQS_trim <- sum(df_trim$WeightedRisk) * (1 + ConcentrationPenalty)

sens_df_trim <- data.frame(
  Scenario = c("Base PQS", "After Removing Top 1% Outliers"),
  PQS = c(PQS_base, PQS_trim)
)

p_trim <- ggplot(sens_df_trim, aes(x = Scenario, y = PQS, fill = Scenario)) +
  geom_col(width = 0.6, alpha = 0.8) +
  scale_fill_manual(values = c("steelblue", "darkgreen")) +
  labs(
    title = "Outlier Removal Sensitivity Test",
    x = "",
    y = "PQS"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

p_trim

