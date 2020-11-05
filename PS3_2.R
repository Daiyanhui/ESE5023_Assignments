library(tidyr)
library(dplyr)
library(ggplot2)

# Read csv
bone_data <- read.csv("bone_data.csv", header=T)
bone_data_tbl <- as_tibble(bone_data) 

ggplot(bone_data_tbl, aes(x = Bone, y = measurement, fill = Bone)) +
  geom_boxplot() +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 10))

anova_one_way <- aov(measurement ~ Bone, data = bone_data_tbl)
summary(anova_one_way)