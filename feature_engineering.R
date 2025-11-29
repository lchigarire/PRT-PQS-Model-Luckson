install.packages("fastDummies")

library(dplyr)
library(fastDummies)
library(scales)

df <- pension_data %>%
  mutate(
    LogBenefit = log(1 + Benefit),
    LogReserve = log(1 + Reserve),
    AgeScaled = rescale(Age),
    BenefitScaled = rescale(Benefit),
    ReserveScaled = rescale(Reserve),
    FundingScaled = rescale(FundingRatio),
    ContributionScaled = rescale(Contribution),
    EnvScaled = rescale(EnvScore)
  )

df2 <- fastDummies::dummy_cols(df,
                               select_columns = c("PlanType",
                                                  "AnnuityType",
                                                  "OccupationClass",
                                                  "LongevityGroup",
                                                  "HealthGroup"),
                               remove_selected_columns = TRUE,
                               remove_first_dummy = TRUE)
#PRINCIPAL COMPONENTS ANALYSIS

library(FactoMineR)
library(factoextra)

num_cols <- df2 %>% select(where(is.numeric))

pca_model <- PCA(num_cols, scale.unit = TRUE, graph = FALSE)

# Add back to original dataframe
df2$PC1 <- pca_model$ind$coord[,1]
df2$PC2 <- pca_model$ind$coord[,2]
df2$PC3 <- pca_model$ind$coord[,3]

fviz_pca_ind(pca_model, 
             geom = "point", 
             pointshape = 21,
             alpha = 0.6)

#CLUSTERING 

set.seed(42)
km <- kmeans(df2[, c("PC1","PC2","PC3")], centers = 5, nstart = 25)

df2$Cluster <- as.factor(km$cluster)

fviz_cluster(list(data=df2[,c("PC1","PC2")], cluster=km$cluster))

#DEVIATION SCORE
centroids <- km$centers

df2$Deviation <- apply(df2[,c("PC1","PC2","PC3")], 1, function(x) {
  k <- df2$Cluster[which(df2$PC1 == x[1] & df2$PC2 == x[2] & df2$PC3 == x[3])[1]]
  centroid <- centroids[as.numeric(k), ]
  sqrt(sum((x - centroid)^2))
})

#vectorize it
df2$Deviation <- sqrt(
  (df2$PC1 - centroids[df2$Cluster, "PC1"])^2 +
    (df2$PC2 - centroids[df2$Cluster, "PC2"])^2 +
    (df2$PC3 - centroids[df2$Cluster, "PC3"])^2
)

#longevity multipliers
df2$LongevityFactor <- scales::rescale(df$LongevityGroup, to=c(0.9, 1.2))

#RISK SCORE
df2$RiskScore <- df2$Deviation * df2$LongevityFactor

#PORTFOLIO QUALITY SCORE
df2$LiabWeight <- df2$Reserve / sum(df2$Reserve)

#ENTROPY PENALTY
cluster_probs <- df2 %>%
  group_by(Cluster) %>%
  summarize(p = n()/nrow(df2))

H <- -sum(cluster_probs$p * log(cluster_probs$p))
Hmax <- log(nrow(cluster_probs))

ConcentrationPenalty <- (Hmax - H) / Hmax
alpha <- 1   

#FINAL SCORE
PQS <- (sum(df2$RiskScore * df2$LiabWeight)) * (1 + alpha * ConcentrationPenalty)
PQS

write.csv(df2, "prt_quality_dataset.csv", row.names = FALSE)



