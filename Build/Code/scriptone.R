#This is a script for R 
#created by; Nicolas Castillo 
#created on 8/25/2021

rm(list=ls())

library(tidyverse)

#function####

  delta<-function(x){
    temp<-((x-lag(x))/lag(x))
    return (temp)
  }
  
  covidIL<-read.csv("./Data/ILCovid19(1).csv")
#view(covidIL)  

covidIL<-covidIL %>% 
  mutate(pc_test = delta(Tests),
         pc_cases = delta(Cases),
         pc_deaths = delta(Deaths))

summary(covidIL)

covidIL$Deaths[is.infinite(covidIL$Deaths)]<-NA

covidIL$Date<-as.Date(covidIL$Date, format = "%m/%d/%Y")

plot(covidIL$Date, covidIL$pc_cases)

plot(covidIL$Date, covidIL$pc_cases,
     main="Percent Cases",
     xlab="",
     ylab="",
     type="l",
     col="blue")
     
     