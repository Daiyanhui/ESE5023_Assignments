Elevation <- c(0.180,0.305,0.381,0.488,0.549,0.640,0.762,0.883)
Temperature <- c(13.3, 12.2, 13.3, 10.0, 8.3, 9.4, 8.3, 7.2)
plot(Temperature ~ Elevation,
     xlab = "Elevation (km)",
     ylab = "Temperature (degrees C)",
     pch = 20,
     cex = 2,
     col = "grey")
fit <- lm(Temperature ~ Elevation)
summary(fit)
abline(fit,lwd=5,col="red")