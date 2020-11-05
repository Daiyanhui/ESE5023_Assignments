library(tidyr)
library(dplyr)
library(ggplot2)

# Read csv
zinc_data <- read.csv("Vegetarian_Zinc.csv", header=T)
zinc_data_tbl <- as_tibble(zinc_data) 

ggplot(zinc_data_tbl, aes(x = Class, y = Zinc_level, fill = Class)) +
  geom_boxplot() +
  theme_bw() +
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15))

anova_one_way <- aov(Zinc_level ~ Class, data = zinc_data_tbl)
summary(anova_one_way)
#TukeyHSD(anova_one_way)