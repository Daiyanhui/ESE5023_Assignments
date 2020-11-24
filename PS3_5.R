library(tidyr)
library(dplyr)
library(ggplot2)
#5.1
data <- read.csv("PS3_5.csv", header=T)
data_tbl <- as_tibble(data) 
plot(Distance ~ Velocity, data=data_tbl,
     xlab = "Velocity",
     ylab = "Distance",
     pch = 20,
     cex = 2,
     col = "black")
#5.2
fit1 <- lm(Distance ~ Velocity, data=data_tbl)
summary(fit1)
abline(fit1,lwd=5,col="blue")
#5.3
# MingYANG noticed：
fit_new<-lm(Distance-0.399098216 ~Velocity,data=hubble_data_tbl)
summary(fit_new)
abline(fit_new, lwd = 5, col = "black")
coef(fit_new)
age<-(fit_new$coefficients[2]*30.9/60/60/24/365)
# the end
