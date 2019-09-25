library(readxl)
library(tidyverse)
library(lubridate)
library(forecast)
library(colortools)

#https://github.com/sciencewithsaurav/Exploratory-Data-Analysis/blob/master/All_Cities.xlsx

dataset <- 
  read_excel("D:/Saurav/DS/MachineHack Pollution Visualization Hackathon/All_Cities.xlsx")

dataset <- dataset %>%
  mutate(date = as.Date(with(dataset, paste(year, month, day, sep="-")),
                   "%Y-%m-%d"))
head(dataset)

avg_SO2 <- mean(dataset$SO2)
avg_NO2 <- mean(dataset$NO2)
avg_PM10 <- mean(dataset$PM10)


#Which state saw the maximum increase in SO2 between 2011 & 2015?
dataset %>% filter(SO2 > 0 & year %in% c("2011","2013","2015")) %>% 
  group_by(State,year) %>% summarize(avg_SO2 = mean(SO2)) %>%
  ggplot(aes(x=reorder(State,avg_SO2),y=avg_SO2, fill=avg_SO2>mean(dataset$SO2))) + 
  geom_bar(stat='identity', position="dodge") + 
  geom_text(aes(label=floor(avg_SO2)),position=position_stack()) +
  #geom_text(aes(label=avg_SO2), vjust=0) 
  geom_hline(yintercept = mean(dataset$SO2), col="red") +
  coord_flip() +
  theme(legend.position = "none") +   #removing legend
  ylab("Average SO2") +
  xlab("States") +
  facet_wrap(~year)


#Display state wise below and abobe concentration (diverging graph)

#Which year saw the maximum increase in SO2, NO2 and PM10?

#Is there any correlation between the change of SO2, NO2 and PM10?

#Is there any abnormal change in SO2, NO2 and PM10 in any month?


