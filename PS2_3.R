library(tidyr)
library(dplyr)
library(ggplot2)
#7.1
data <- read.csv(file="PS1_7.csv",header = T)
data1 <- as_tibble(data)
data1_valid <- data1%>%
  mutate(Year = V04001) %>%
  mutate(Month = V04002) %>%
  mutate(temp_data = ifelse(V12001_701!=999999 , V12001_701, NA)) %>% #温度数据
  mutate(wind_data = ifelse(V11291_701!=999999 , V11291_701, NA)) %>%  #风速数据
  mutate(p_data = ifelse(V13305!=999999 , V13305, NA)) %>% #降水量数据
  select(Year,Month,temp_data,wind_data,p_data) 
#7.2
data1_valid %>%
  select(Year,Month,temp_data)%>%
  filter(Year == 1981)%>%
  ggplot(aes(x=Month, y=temp_data))+
  geom_line()
#7.3
#1)
data1_valid %>%
  select(Year,temp_data)%>%
  group_by(Year)%>%
  summarize(yearly_mean = mean(temp_data,na.rm = TRUE))%>%
  ggplot(aes(x=Year, y=yearly_mean))+
  geom_line()

#2)
data1_valid %>%
  select(Year,p_data)%>%
  group_by(Year)%>%
  summarize(yearly_mean = mean(p_data,na.rm = TRUE))%>%
  ggplot(aes(x=Year, y=yearly_mean))+
  geom_line()
#3)
data1_valid %>%
  select(Year,Month,temp_data)%>%
  group_by(Month)%>%
  summarize(Monthly_mean = mean(temp_data,na.rm = TRUE))%>%
  #mutate(month=Month[which(Monthly_mean==max(Monthly_mean,na.rm=TRUE))])%>%
  ggplot(aes(x=Month, y=Monthly_mean))+
  geom_line() # 每个月1981-2018年多年平均温度


#4)
data1_valid %>%
  select(Year,Month,p_data)%>%
  group_by(Month)%>%
  summarize(Monthly_mean = mean(p_data,na.rm = TRUE))%>%
  #mutate(month=Month[which(Monthly_mean==max(Monthly_mean,na.rm=TRUE))])%>%
  ggplot(aes(x=Month, y=Monthly_mean))+
  geom_line() # 每个月1981-2018年多年平均降雨

#5）
data1_valid %>%
  select(Year,Month,wind_data)%>%
  group_by(Month)%>%
  summarize(Monthly_mean = mean(wind_data,na.rm = TRUE))%>%
  #mutate(month=Month[which(Monthly_mean==max(Monthly_mean,na.rm=TRUE))])%>%
  ggplot(aes(x=Month, y=Monthly_mean))+
  geom_line() # 每个月1981-2018年多年平均风速

  
