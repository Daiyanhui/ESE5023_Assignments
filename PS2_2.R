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
  