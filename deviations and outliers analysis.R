library(dplyr)
library(ggplot2)
library(scales)

p_dev_hist <- ggplot(df2, aes(x = Deviation)) +
  geom_histogram(aes(y = ..density..),
                 bins = 40,
                 color = "white",
                 fill  = "steelblue",
                 alpha = 0.7) +
  geom_density(color = "red", size = 0.8) +
  labs(
    title = "Distribution of Deviation Scores",
    x = "Deviation (distance to cluster centroid in PCA space)",
    y = "Density"
  ) +
  theme_minimal()

p_dev_hist
ggsave("deviation_histogram.png", p_dev_hist,
       width = 7, height = 4.5, dpi = 300)
p_dev_hist


