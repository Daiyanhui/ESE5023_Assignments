library(tidyr)
library(dplyr)
library(ggplot2)
data <- read.csv(file="2281305.csv",header = T)
data1 <- as_tibble(data)
data_valid <- data1%>%
  mutate(Year = as.numeric(substr(DATE,1,4))) %>%
  mutate(Month = as.numeric(substr(DATE,6,7))) %>%
  mutate(speed_rate = as.numeric(substr(WND,9,12))) %>%
  mutate(speed_QC = as.numeric(substr(WND,14,14))) %>%
  mutate(type_code = substr(WND,7,7)) %>%
  select(Year,Month,speed_rate,speed_QC,type_code) %>%
  mutate(speed_rate_new = ifelse(speed_QC==1 & type_code == "N" & speed_rate!=9999, speed_rate, NA))
data_valid %>%
  select(Year,Month,speed_rate_new) %>%
  group_by(Year,Month) %>%
  summarize(Monthly_mean_speed = mean(speed_rate_new,na.rm=TRUE))%>%
  ggplot(aes(x=Year, y=Monthly_mean_speed)) + 
  geom_line() +
  facet_wrap(~ Month)
  
# @MingYANG noticed:
# to select the qualified data, function "filter" should be used properly
# try and understand this:
# a_Data       <- read.csv(file = "2281305.csv", header = T)
# a_Data <- as_tibble(a_Data)
# a_Data%>% 
#   select(WND,DATE) %>% 
#   filter(substr(WND,1,3)!="999") %>% 
#   filter(substr(WND,9,12)!="9999") %>% 
#   filter(substr(WND,5,7)=="1,N")%>%
#   filter(substr(WND,14,14)=="1")%>%
#   mutate(Month=as.character(paste0(substr(DATE,1,4),"-",substr(DATE,6,7)))) %>%
#   mutate(wind_speed=as.numeric(substr(WND,9,12)))%>%
#   select(Month,wind_speed) %>% 
#   group_by(Month) %>% 
#   summarise(windspeed_month=mean(wind_speed,na.rm = T)) %>% 
#   mutate(month = as.Date(paste0(Month,"-","15"))) %>%
#   ggplot(aes(x=month, y=windspeed_month)) + 
#  geom_line()
