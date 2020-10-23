library(tidyr)
library(dplyr)
library(ggplot2)
#1.1
# load the txt data into csv
signif_data <- read.csv(file = "signif.csv",header = T)
Sig_Eqs <- as_tibble(signif_data)
#1.2
Sig_Eqs %>%
  select(YEAR, COUNTRY, DEATHS)%>%
  group_by(COUNTRY)%>%
  summarize(total_number_deaths = sum(DEATHS,na.rm = TRUE)) %>%
  arrange(desc(total_number_deaths))
#1.3
Sig_Eqs %>%
  select(YEAR, COUNTRY, EQ_PRIMARY)%>%
  filter(EQ_PRIMARY > 6.0) %>%
  group_by(YEAR) %>%
  summarize(total_number_EQ = n(),na.rm=TRUE) %>%
  ggplot(aes(x=YEAR, y=total_number_EQ))+
  geom_line()
#1.4
CountEq_LargestEq <- function(country_name) {
  total_number <- Sig_Eqs %>%
    select(YEAR,MONTH,DAY,COUNTRY,EQ_PRIMARY) %>%
    filter(COUNTRY == country_name) %>%
    group_by(COUNTRY) %>%
    summarize(t_N_EQ = n(),
              YEAR = YEAR[which(EQ_PRIMARY == max(EQ_PRIMARY,na.rm=TRUE))],
              MONTH = MONTH[which(EQ_PRIMARY == max(EQ_PRIMARY,na.rm=TRUE))],
              DAY = DAY[which(EQ_PRIMARY == max(EQ_PRIMARY,na.rm=TRUE))])
  print(total_number)
}
Country_names <- Sig_Eqs%>%
  select(COUNTRY)
num <- length(Country_names)
for (i in 1:num) {
  Results<-CountEq_LargestEq(Country_names[i])
}
Results_desc<-Results%>%
  #group_by(COUNTRY)%>%
  arrange(desc(t_N_EQ))


  
