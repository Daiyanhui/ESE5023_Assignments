#1.1 Plot two box plots side-by-side of data from the two groups.Discribe the distributions.
library(dplyr)
library(tidyr)
library(ggplot2)

data <- read.csv(file = "cloud_seed.csv", header = T)
data_tbl <- as_tibble(data)

boxplot(Rainfall ~ Label,data=data_tbl)
# or
#ggplot(data_tbl, aes(x = Label, y = Rainfall, fill = Label)) +
 # geom_boxplot() +
  #theme_classic()

#1.2 Did cloud seeding have an effect on rainfall in this experiment? If so, how much?
anova_one_way <- aov(Rainfall ~ Label, data = data_tbl)
summary(anova_one_way)

# good work
