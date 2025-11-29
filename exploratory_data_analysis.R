str(portfolio)
summary(portfolio)

install.packages("skimr")
library(skimr)

skimr::skim(portfolio)

table(portfolio$CaseName)
table(portfolio$Gender)
table(portfolio$Type)
table(portfolio$PlanType)
table(portfolio$HealthGroup)
table(portfolio$Occupation)

library(ggplot2)

#Age distribution
ggplot(portfolio, aes(x=Age, fill = CaseName)) +
  geom_histogram(binwidth = 2, position = "dodge")+
  theme_minimal()

#Premium Distribution
ggplot(portfolio, aes(x=Premium, fill=CaseName))+
  geom_density(alpha=0.4) +
  scale_x_log10() +
  theme_minimal()

#Premium By Case
ggplot(portfolio, aes(x=CaseName, y=Premium))+
  geom_boxplot()+
  scale_y_log10()+
  theme_minimal()

#Reserve By Health Group
ggplot(portfolio, aes(x=HealthGroup, y= Reserve))+
  geom_boxplot()+
  scale_y_log10()+
  theme_minimal()

#Reserve Per Benefit Amount
portfolio$ReserveToBenefit <- portfolio$Reserve / portfolio$BenefitAmount

ggplot(portfolio, aes(x=CaseName, y= ReserveToBenefit))+
  geom_boxplot()+
  theme_minimal()

#Heatmap of Longevity Risk Outliers
ggplot(portfolio, aes(x=LongevityGroup, y=HealthGroup))+
  geom_bar(position = "fill")+
  facet_wrap(~CaseName)+
  labs(y="proportion", title = "Longevity Vs Health By Case")+
  theme_minimal()

#Detect Outliers
boxplot(portfolio$Premium ~ portfolio$CaseName, log = "y")


