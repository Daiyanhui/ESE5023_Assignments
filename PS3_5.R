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
