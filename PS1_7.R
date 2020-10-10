#7.1
data <- read.csv(file="PS1_7.csv",header = T)
temp_data <- data$V12001_701 #温度数据
wind_data <- data$V11291_701 #风速数据
p_data <- data$V13305 #降水量数据
date <- data$V04001 #时间
month <- data $V04002 #月份
temp_data[which(temp_data == 999999)] <- NA
wind_data[which(wind_data == 999999)] <- NA
p_data[which(p_data == 999999)] <- NA
#7.2
temp_data_1981 <- temp_data[which(date == 1981)]
plot(seq(1,12,1),temp_data_1981,type = "l",col="blue",lwd="0.1",xlab = "Month",ylab = "Temperature")
#7.3
#1)
mean_temp <- array(NA,38)
i <- 1
for (j in 1981:2018) {
  mean_temp[i] <- mean(temp_data[which(date == j)],na.rm = T)
  i<-i+1
}
plot(seq(1981,2018,1),mean_temp,type = "l",col="blue",lwd="0.1",xlab = "Year",ylab = "Temperature")
#2)
mean_p <- array(NA,38)
i <- 1
for (j in 1981:2018) {
  mean_p[i] <- mean(p_data[which(date == j)],na.rm = T)
  i<-i+1
}
plot(seq(1981,2018,1),mean_p,type = "l",col="blue",lwd="0.1",xlab = "Year",ylab = "Precipitation")
#3)
mean_month_temp <-array(NA,12)
i <-1
for (j in 1:12) {
    mean_month_temp[i] <- mean(temp_data[which(month == j)],na.rm = T)
    i<-i+1
}
plot(seq(1,12,1),mean_month_temp,type = "l",col="blue",lwd="0.1",xlab = "Month",ylab = "Temperature")
max_month <- which(mean_month_temp==max(mean_month_temp)) #温度最高的月份
#4)
mean_month_p <-array(NA,12)
i <-1
for (j in 1:12) {
  mean_month_p[i] <- mean(p_data[which(month == j)],na.rm = T)
  i<-i+1
}
plot(seq(1,12,1),mean_month_p,type = "l",col="blue",lwd="0.1",xlab = "Month",ylab = "Precipitation")
max_month <- which(mean_month_p==max(mean_month_p)) #降水量最高的月份
#5）
mean_month_w <-array(NA,12)
i <-1
for (j in 1:12) {
  mean_month_w[i] <- mean(wind_data[which(month == j)],na.rm = T)
  i<-i+1
}
plot(seq(1,12,1),mean_month_w,type = "l",col="blue",lwd="0.1",xlab = "Month",ylab = "Wind speed")
max_month <- which(mean_month_w==max(mean_month_w)) #风速最高的月份