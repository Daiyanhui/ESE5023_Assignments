#The parts of Time Series was inspired by TaoTaoSun and Li Yuan
#I learned the code from Li
#This part,I haven't grasp it.

library(dplyr)
library(lubridate)
library(forecast)

data_tbl <- as_tibble(read.csv(file = "2281305.csv", header = T))

data_tbl <- data_tbl %>%
  select(DATE,TMP) %>%
  mutate(
    T_value = as.numeric(substr(TMP,1,5)),
    T_flag = as.logical(as.numeric(substr(TMP,7,7)))
  ) %>% 
  
  filter(T_value!= 9999 & T_flag==TRUE) %>% 
  mutate(T_value2 = T_value * 0.1,
         Obs_Time = as.Date(DATE))

data_tbl <- data_tbl %>%
  select(Obs_Time,T_value2) %>%
  group_by(Obs_Time) %>%
  summarise(T_value_monthly = mean(T_value2)) 

plot(data_tbl$Obs_Time,data_tbl$T_value_monthly, 
     type="l",xlab="Date",ylab="BaoAn_T_value")

#2.1 time series from 2010 Jan. to 2020 Aug.
monthly_averaged_temp_ts <- ts(data_tbl$T_value_monthly, 
                               start=c(2010,1), end=c(2020,8),frequency=12)
# Check structure
str(monthly_averaged_temp_ts)
# Plot time series
plot(monthly_averaged_temp_ts, type="l")

#2.2 Decompose the time series into trend, seasonality, 
#and error parts. 
#-------------------------------------------------
time_series_components <- decompose(monthly_averaged_temp_ts)
plot(time_series_components)

hist(time_series_components$random, prob=TRUE)

curve(dnorm(x, mean=mean(time_series_components$random,
                         na.rm=T),sd=sd(time_series_components$random,na.rm=T)),
      add=TRUE, col="green")


#2.3 Fit an ARIMA(p,d,q) model to the time series. 
trModel <- lm(monthly_averaged_temp_ts ~ c(1:length(monthly_averaged_temp_ts)))
plot(resid(trModel), type="l")

acf(resid(trModel))
pacf(resid(trModel))

auto.arima(monthly_averaged_temp_ts,trace=T)

arima1 <- arima(monthly_averaged_temp_ts,order=c(1,0,0),seasonal=list(order=c(1,0,0),period=12),method="ML")
arima1
arima2 <- arima(monthly_averaged_temp_ts,order=c(1,0,0),seasonal=list(order=c(2,1,0),period=12),method="ML")
arima2


#2.4 Predict monthly-averaged temperatures in 2020 Sep. and Oct
model <- arima2
months_forecast  <- 2
months_in_plot <- 20
forecast_2months <- forecast(model, months_forecast)

# Plot predictions along with real values
plot(forecast(model, months_forecast), include = months_in_plot, 
     xlab="Time", 
     ylab="Temperature",type="o",lwd=1,
     col = "blue") 

# MingYANG noticed:
# good work but there is no necessary explain in your report
# the end
