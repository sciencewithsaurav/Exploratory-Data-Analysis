library(readxl)
library(tidyverse)
library(lubridate)
library(forecast)
library(colortools)

dataset <- 
  read_excel("D:/Saurav/DS/MachineHack Pollution Visualization Hackathon/All_Cities.xlsx")

dataset <- dataset %>%
  mutate(date = as.Date(with(dataset, paste(year, month, day, sep="-")),
                   "%Y-%m-%d"))
head(dataset)

avg_SO2 <- mean(dataset$SO2)
avg_NO2 <- mean(dataset$NO2)
avg_PM10 <- mean(dataset$PM10)

#How does SO2, NO2 and PM10 change over time for each state?
p <- dataset %>% group_by(State) %>%
  ggplot(aes(x = date, y = PM10)) +
  geom_smooth(method = lm, se = FALSE,col="red") +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  geom_hline(yintercept = mean(dataset$PM10), col="green") +
  geom_text(aes(mean(date),mean_PM10,label = "Mean PM10", vjust = 0.5))+
  theme_classic()
#p + geom_line()

#Display state wise below and abobe concentration (diverging graph)

#Which state saw the maximum increase in SO2, NO2 and PM10?
dataset %>% filter(SO2 > 0) %>% 
  group_by(State) %>% summarize(avg_SO2 = mean(SO2)) %>%
  ggplot(aes(x=reorder(State,avg_SO2),y=avg_SO2, fill=avg_SO2>mean(dataset$SO2))) + 
  geom_bar(stat='identity') + 
  #geom_text(aes(label=avg_SO2), vjust=0) 
  geom_hline(yintercept = mean(dataset$SO2), col="red") +
  coord_flip() +
  theme(legend.position = "none") +   #removing legend
  ylab("Average SO2") +
  xlab("States")

#Which year saw the maximum increase in SO2, NO2 and PM10?

#Is there any correlation between the change of SO2, NO2 and PM10?

#Is there any abnormal change in SO2, NO2 and PM10 in any month?


