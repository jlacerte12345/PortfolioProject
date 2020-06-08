rm(list=ls())
library(readxl)
library(dplyr)

##Read Files into Dataframes
dfAsthma2014 = read.csv("asthma2014.csv")
dfAsthma2018 = read.csv("asthma2018.csv")
dfAir2014 = read.csv("airquality2014.csv")
dfAir2018 = read.csv("airquality2018.csv")

#summary statistics
summary(dfAsthma2014)
summary(dfAsthma2018)
summary(dfAir2014)
summary(dfAir2018)


##Clean data, only need the state and their mean ozone levels
dfAir2014 = dfAir2014 %>% select(State,Days.Ozone)
dfAir2018 = dfAir2018 %>% select(State,Days.Ozone)

dfAir2014 = dfAir2014 %>% 
  group_by(State) %>%
  summarise(mean = mean(Days.Ozone))

dfAir2014 = as.data.frame(dfAir2014)
dfAir2014$Mean2014 = dfAir2014$mean

dfAir2018 = dfAir2018 %>% 
  group_by(State) %>%
  summarise(mean = mean(Days.Ozone))

dfAir2018 = as.data.frame(dfAir2018)
dfAir2018$Mean2018 = dfAir2018$mean

#Compare 2014 and 2018 values in T-Test for both Air and Asthma

##Air
Mean2014 = dfAir2014$Mean2014
Mean2018 = dfAir2018$Mean2018

OzoneT = t.test(Mean2014,Mean2018)
OzoneT

##Asthma
AsthmaPercent2014 = dfAsthma2014$Percent.with.Current.Asthma
AsthmaPercent2018 = dfAsthma2018$Percent.with.Current.Asthma

AsthmaT = t.test(AsthmaPercent2014,AsthmaPercent2018)
AsthmaT
