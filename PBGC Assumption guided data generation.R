set.seed(123)
n <- 20000

pension_data <- data.frame(UniqueID = sprintf("ID%05d", 1:n))
pension_data$AnnuityType <- sample(c("Retiree", "Deferred"), n, replace=TRUE, prob=c(0.6, 0.4))
pension_data$Gender <- NA
pension_data$Gender[pension_data$AnnuityType == "Retiree"] <- sample(c("Male", "Female"),
                                                                     sum(pension_data$AnnuityType=="Retiree"), replace=TRUE, prob=c(0.63,0.37))
pension_data$Gender[pension_data$AnnuityType == "Deferred"] <- sample(c("Male", "Female"),
                                                                      sum(pension_data$AnnuityType=="Deferred"), replace=TRUE, prob=c(0.6,0.4))
ret_index <- pension_data$AnnuityType == "Retiree"
def_index <- pension_data$AnnuityType == "Deferred"
pension_data$Age <- NA
pension_data$Age[ret_index] <- round(rnorm(sum(ret_index), mean=75, sd=8))
pension_data$Age[ret_index] <- pmax(pension_data$Age[ret_index], 65)
pension_data$Age[ret_index] <- pmin(pension_data$Age[ret_index], 100)

pension_data$Age[def_index] <- round(rnorm(sum(def_index), mean=55, sd=8))
pension_data$Age[def_index] <- pmax(pension_data$Age[def_index], 25)
pension_data$Age[def_index] <- pmin(pension_data$Age[def_index], 64)
base_mean <- 600
base_sd <- 150
pension_data$BaseBenefit <- rlnorm(n, meanlog=log(base_mean), sdlog=log(1 + base_sd/base_mean))
plan_types <- c("Single Life", "Life 10-Year Certain", "Joint 50% Survivor")
pension_data$PlanType <- sample(plan_types, n, replace=TRUE, prob=c(0.5, 0.2, 0.3))
pension_data$Benefit <- pension_data$BaseBenefit
pension_data$Benefit[pension_data$PlanType == "Joint 50% Survivor"] <- 
  pension_data$Benefit[pension_data$PlanType == "Joint 50% Survivor"] * 0.8
pension_data$Benefit[pension_data$PlanType == "Life 10-Year Certain"] <- 
  pension_data$Benefit[pension_data$PlanType == "Life 10-Year Certain"] * 0.95


current_year <- 2024
pension_data$RetirementYear <- NA
pension_data$RetirementYear[ret_index] <- current_year - (pension_data$Age[ret_index] - 65)

pension_data$BenefitStartYear <- NA
pension_data$BenefitStartYear[ret_index] <- pension_data$RetirementYear[ret_index] + 
  sample(0:1, sum(ret_index), replace=TRUE)
pension_data$BenefitStartYear[def_index] <- current_year + (65 - pension_data$Age[def_index])

pension_data$LongevityGroup <- sample(1:5, n, replace=TRUE)
pension_data$HealthGroup <- NA
pension_data$HealthGroup[pension_data$Age < 65] <- sample(1:3, sum(pension_data$Age < 65), replace=TRUE, prob=c(0.6,0.3,0.1))
pension_data$HealthGroup[pension_data$Age >= 65 & pension_data$Age < 80] <- 
  sample(1:3, sum(pension_data$Age >= 65 & pension_data$Age < 80), replace=TRUE, prob=c(0.3,0.5,0.2))
pension_data$HealthGroup[pension_data$Age >= 80] <- sample(1:3, sum(pension_data$Age >= 80), replace=TRUE, prob=c(0.1,0.3,0.6))

pension_data$OccupationClass <- sample(c("White Collar", "Blue Collar"), n, replace=TRUE, prob=c(0.6, 0.4))

zip5 <- sprintf("%05d", sample(10000:99999, n, replace=TRUE))
zip4 <- sprintf("%04d", sample(1000:9999, n, replace=TRUE))
pension_data$ZIP <- paste0(zip5, "-", zip4)

pension_data$FundingRatio <- round(runif(n, 0.6, 1.3), 3)
pension_data$Contribution <- round(runif(n, 1000, 10000), 0)
pension_data$EnvScore <- sample(1:100, n, replace=TRUE)
age_factor <- 0.5 + (80 - pension_data$Age)/50
age_factor <- pmax(age_factor, 0.3)
pension_data$Premium <- pension_data$Benefit * age_factor

pension_data$Premium[pension_data$PlanType == "Joint 50% Survivor"] <- 
  pension_data$Premium[pension_data$PlanType == "Joint 50% Survivor"] * 1.2
pension_data$Premium[pension_data$PlanType == "Life 10-Year Certain"] <- 
  pension_data$Premium[pension_data$PlanType == "Life 10-Year Certain"] * 1.1

pension_data$Reserve <- NA
for(i in seq_len(n)) {
  age <- pension_data$Age[i]
  ben <- pension_data$Benefit[i]
  gender <- pension_data$Gender[i]
  if(pension_data$AnnuityType[i] == "Retiree") {
    base <- ifelse(gender == "Male", 80, 82)
    life_exp <- max(base - age, 1)
    disc <- (1.03^life_exp)
    pension_data$Reserve[i] <- ben * 12 * life_exp / disc
  } else {
    base_at65 <- ifelse(gender == "Male", 80, 82)
    life_exp_at65 <- max(base_at65 - 65, 1)
    wait <- max(65 - age, 0)
    disc_wait <- (1.03^wait)
    pension_data$Reserve[i] <- ben * 12 * life_exp_at65 / disc_wait
  }
}

pension_data$Benefit <- round(pension_data$Benefit, 2)
pension_data$Premium <- round(pension_data$Premium, 2)
pension_data$Reserve <- round(pension_data$Reserve, 2)

head(pension_data)

write.csv(pension_data, "synthetic_prt_dataset.csv", row.names = FALSE)

#Exploratory Data Analysis
library(ggplot2)
library(dplyr)

install.packages("plotly")
install.packages("corrplot")

library(plotly)
library(corrplot)

# Quick overview
str(pension_data)
summary(pension_data)

# Categorical summaries
table(pension_data$AnnuityType)
table(pension_data$PlanType)
table(pension_data$Gender)
table(pension_data$OccupationClass)
table(pension_data$HealthGroup)
table(pension_data$LongevityGroup)

ggplot(pension_data, aes(x = Age, fill = AnnuityType)) +
  geom_histogram(bins = 30, position = "identity", alpha = 0.6) +
  labs(title = "Age Distribution by Annuity Type", x = "Age", y = "Count")

ggplot(pension_data, aes(x = Benefit)) +
  geom_histogram(bins = 50, fill = "steelblue", color = "black") +
  labs(title = "Monthly Benefit Distribution", x = "Monthly Benefit ($)", y = "Count")

ggplot(pension_data, aes(x = PlanType, y = Reserve, fill = PlanType)) +
  geom_boxplot() +
  labs(title = "Reserve by Plan Type", x = "Plan Type", y = "Reserve ($)")

ggplot(pension_data, aes(x = factor(HealthGroup), y = Benefit, fill = factor(HealthGroup))) +
  geom_boxplot() +
  labs(title = "Benefit by Health Group", x = "Health Group (1 = Best)", y = "Monthly Benefit")

ggplot(pension_data, aes(x = OccupationClass)) +
  geom_bar(fill = "darkorange") +
  labs(title = "Count by Occupation Class", x = "Occupation Class", y = "Count")


numeric_vars <- pension_data %>%
  select(Age, Benefit, Premium, Reserve, FundingRatio, Contribution, EnvScore)

cor_matrix <- cor(numeric_vars, use = "complete.obs")

# Visualize correlation matrix
corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.8, addCoef.col = "black")

ggplot(pension_data, aes(x = Age, y = Reserve, color = PlanType)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "loess") +
  labs(title = "Reserve by Age and Plan Type", x = "Age", y = "Reserve ($)")


#Extracting ZIP Prefix

pension_data$ZIP3 <- substr(pension_data$ZIP, 1, 3)

# Count by ZIP3
zip_summary <- pension_data %>%
  group_by(ZIP3) %>%
  summarise(
    Count = n(),
    AvgBenefit = mean(Benefit),
    AvgReserve = mean(Reserve),
    AvgAge = mean(Age),
    AvgFundingRatio = mean(FundingRatio)
  ) %>%
  arrange(desc(Count))

head(zip_summary)


ggplot(zip_summary %>% top_n(20, Count), aes(x = reorder(ZIP3, Count), y = Count)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  coord_flip() +
  labs(title = "Top 20 ZIP3 Regions by Participant Count", x = "ZIP Prefix (ZIP3)", y = "Participant Count")


ggplot(zip_summary, aes(x = ZIP3, y = 1, fill = AvgBenefit)) +
  geom_tile() +
  scale_fill_viridis_c() +
  theme_minimal() +
  labs(title = "Average Monthly Benefit by ZIP3", x = "ZIP Prefix", y = "", fill = "Avg Benefit ($)") +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

# Create region risk score based on benefit/reserve ratios or thresholds
zip_summary <- zip_summary %>%
  mutate(
    RegionRisk = case_when(
      AvgBenefit > 900 & AvgFundingRatio < 0.8 ~ "High",
      AvgBenefit > 700 & AvgFundingRatio < 1.0 ~ "Medium",
      TRUE ~ "Low"
    )
  )

table(zip_summary$RegionRisk)
