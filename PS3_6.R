#6.1
library(MASS)
data(cpus)
#str(cpus)
sample_index <- sample(nrow(cpus),nrow(cpus)*0.80)
cpus_train <- cpus[sample_index,]
cpus_test  <- cpus[-sample_index,]
model <- lm(perf ~ syct + mmin + mmax + cach + chmin + chmax, data = cpus_train)
summary(model)
#coef(model)
#6.2
cpus_test1 <- cpus_test %>%
  mutate(perf_pre = 0.05809*syct + 0.01607*mmin + 0.006185*mmax + 0.4635*cach - 0.4756*chmin + 1.538*chmax - 64.9737)
mean_bias = mean(abs(cpus_test1$perf - cpus_test1$perf_pre))
print(mean_bias)
plot(perf_pre ~ perf, data=cpus_test1,
     main = "Provided perf vs Predicted perf",
     xlab = "perf",
     ylab = "perf_pre",
     pch = 20,
     cex = 2,
     col = "green")

fit2 <- lm(perf_pre ~ perf, data=cpus_test1)
summary(fit2)
abline(fit2,lwd=3,col="blue")
text(150,600, "Mean Bias = 39.03")
