data <- read.csv(file="2281305.csv",header = T)
vis_data <- data$VIS
Obs_Time <- data$DATE
Obs_Time2 <- as.Date(Obs_Time)

vis_data_value <- substr(vis_data,1,6)
vis_data_value2 <- as.numeric(vis_data_value)

distance_q <- substr(vis_data,8,8)
distance_q2 <- as.numeric(distance_q)

variability_code <- substr(vis_data,10,10)

quality_variability_code <- substr(vis_data,12,12)
quality_variability_code2 <- as.numeric(quality_variability_code)

vis_data_value2[which(vis_data_value2 == 999999)] <- NA
vis_data_value2[which(distance_q2 != 1)] <- NA
vis_data_value2[which(quality_variability_code2 != 1)] <- NA
vis_data_value2[which(variability_code != "N")] <- NA
Obs_Time2[which(vis_data_value2 == 999999)] <- NA
Obs_Time2[which(distance_q2 != 1)] <- NA
Obs_Time2[which(quality_variability_code2 != 1)] <- NA
Obs_Time2[which(variability_code != "N")] <- NA

plot(Obs_Time2,vis_data_value2,type = "l",col="blue",lwd="0.1")
#*******************************************************************
Obs_Time3 <- substr(Obs_Time,1,4)
Obs_Time3 <- as.numeric(Obs_Time3)

n_0_5 <- array(0,4)
n_5_10 <- array(0,4)
n_10_15 <- array(0,4)
n_15_20 <- array(0,4)
n_20_25 <- array(0,4)
n_25_30 <- array(0,4)
n_30 <- array(0,4)

for (i in 2010:2013) {
  year_data <- vis_data_value2[which(Obs_Time3 == i)]
  num <- length(year_data)
  for (j in 1:num) {
    if(year_data[j] >= 0 && year_data[j] < 5000) {
      n_0_5[i-2009] <- n_0_5[i-2009]+1
    }
    if(year_data[j] >= 5000 && year_data[j] < 10000) {
      n_5_10[i-2009] <- n_5_10[i-2009]+1
    }
    if(year_data[j] >= 10000 && year_data[j] < 15000) {
      n_10_15[i-2009] <- n_10_15[i-2009]+1
    }
    if(year_data[j] >= 15000 && year_data[j] < 20000) {
      n_15_20[i-2009] <- n_15_20[i-2009]+1
    }
    if(year_data[j] >= 20000 && year_data[j] < 25000) {
      n_20_25[i-2009] <- n_20_25[i-2009]+1
    }
    if(year_data[j] >= 25000 && year_data[j] < 30000) {
      n_25_30[i-2009] <- n_25_30[i-2009]+1
    }
    if(year_data[j] >= 30000 && year_data[j] <= 160000) {
      n_30[i-2009] <- n_30[i-2009]+1
    }
  }
 
}
print(n_0_5)
print(n_5_10)
print(n_10_15)
print(n_15_20)
print(n_20_25)
print(n_25_30)
print(n_30)
  
 

