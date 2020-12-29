library(tidyverse)
library(ggpubr) 
theme_set(
  theme_bw() + 
    theme(legend.position = "top")
)


#1.1 scatter plot
library("ggpubr")
p <- ggplot(mtcars, aes(mpg, wt)) +
  geom_point() +
  geom_smooth(method = lm) +
  stat_cor(method = "pearson", label.x = 20)
p

#use library(ggforce)
library(ggforce)
ggplot(iris, aes(Petal.Length, Petal.Width, colour = Species)) +
  geom_point() +
  facet_zoom(x = Species == "versicolor")


# Basic scatter plot
ggplot(mpg, aes(cty, hwy)) +
  geom_point(size = 0.5)

# Jittered points
ggplot(mpg, aes(cty, hwy)) +
  geom_jitter(size = 0.5, width = 0.5)


# 1.2 box plot
# Use box plot as marginal plots
ggscatterhist(
  iris, x = "Sepal.Length", y = "Sepal.Width",
  color = "Species", size = 3, alpha = 0.6,
  palette = c("#00AFBB", "#E7B800", "#FC4E07"),
  margin.plot = "boxplot",
  ggtheme = theme_bw()
)



#1.3 Histogram
ggplot(iris, aes(Sepal.Length)) +
  geom_histogram(aes(fill = Species, color = Species), bins = 20, 
                 position = "identity", alpha = 0.5) +
  scale_fill_viridis_d() +
  scale_color_viridis_d()


# 1.4 Time series
library(ggplot2)
library(dplyr)

# Dummy data
data <- data.frame(
  day = as.Date("2017-06-14") - 0:364,
  value = runif(365) + seq(-140, 224)^2 / 10000
)

# Most basic bubble plot
p <- ggplot(data, aes(x=day, y=value)) +
  geom_line() + 
  xlab("")
p



# 1.5 image plot
#Yiwen introduce this method to me,and I learned the code from him
sample = cor(matrix(rnorm(400),nrow = 20))
image(cor(matrix(rnorm(400),nrow = 20)),axes = F)
mtext(text = c(paste("country",1:21)),side = 2,line = 0.3,at = seq(0,1,0.05),las = 1,cex = 0.8)
mtext(text = c(paste("country",1:21)),side = 1,line = 0.3,at = seq(0,1,0.05),las = 2,cex = 0.8)
image.plot(sample,legend.only = T)

# MingYANG noticed:
# Time series plot is too simple
# good work
