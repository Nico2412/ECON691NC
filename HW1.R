#This is a script for R 
#created by; Nicolas Castillo 
#created on 8/25/2021

rm(list=ls())

library(tidyverse)
attach(covidIL)
#function####

DIF<-function(Date){
  temp<-((Date-lag(Date))/lag(Date))
  return(round(temp2,4))
}   ####this is the function i used##



covidIL<-read.csv("./Data/ILCovid19(1).csv")
view(covidIL)  


covidIL<-covidIL %>% 
  mutate(New_test = DIF(Tests),
         New_cases = DIF(Cases),
         New_deaths = DIF(Deaths))
##I created new colums with the function##
view(covidIL)

covidIL$New_deaths[is.infinite(covidIL$New_deaths)]<-NA
covidIL$New_deaths[is.nan(covidIL$New_deaths)]<-NA
covidIL$New_deaths[is.na(covidIL$New_deaths)]<-0

covidIL$New_cases[is.infinite(covidIL$New_cases)]<-NA
covidIL$New_cases[is.nan(covidIL$New_cases)]<-NA
covidIL$New_cases[is.na(covidIL$New_cases)]<-0

covidIL$New_test[is.infinite(covidIL$New_test)]<-NA
covidIL$New_test[is.nan(covidIL$New_test)]<-NA
covidIL$New_test[is.na(covidIL$New_test)]<-0

##I basically turned every infinite to NA then every Nan to Na and finally 
#Na to 0


view(covidIL)


DIF2<-function(x){
  temp2<-((x-lag(x))/lag(x))
  return(round(temp2,4))
}

#then i created a second function for percentage change
covidIL<-covidIL %>% 
  mutate(PercNew_test = DIF2(New_test),
         PercNew_cases = DIF2(New_cases),
         PercNew_deaths = DIF2(New_deaths))
## added new columns to show percentage change
view(covidIL)

covidIL$PercNew_deaths[is.infinite(covidIL$PercNew_deaths)]<-NA
covidIL$PercNew_deaths[is.nan(covidIL$PercNew_deaths)]<-NA
covidIL$PercNew_deaths[is.na(covidIL$PercNew_deaths)]<-0

covidIL$PercNew_cases[is.infinite(covidIL$PercNew_cases)]<-NA
covidIL$PercNew_cases[is.nan(covidIL$PercNew_cases)]<-NA
covidIL$PercNew_cases[is.na(covidIL$PercNew_cases)]<-0

covidIL$PercNew_test[is.infinite(covidIL$PercNew_test)]<-NA
covidIL$PercNew_test[is.nan(covidIL$PercNew_test)]<-NA
covidIL$PercNew_test[is.na(covidIL$PercNew_test)]<-0
##I basically turned every infinite to NA then every Nan to Na and finally 
#Na to 0
view(covidIL)

covidIL$Date<-as.Date(covidIL$Date, format = "%m/%d/%Y")

plot(covidIL$Date, covidIL$PercNew_cases,
     main=" Percentage Change in New Cases",
     xlab="Date",
     ylab="Percentage change of New cases",
     type="l",
     ylim = c(0,3),
     xlim = as.Date(c("2020-03-10","2021-08-25")),
     col="green") 

#created the graph with the specific requirements 

plot(covidIL$Date, covidIL$PercNew_deaths,
     main="Percentage Change in New Deaths",
     xlab="Date",
     ylab="Percentage change of New Deaths",
     type="l",
     ylim = c(0,24),
     xlim = as.Date(c("2020-03-10","2021-08-25")),
     col="blue") 


plot(covidIL$Date, covidIL$PercNew_test,
     main="Percentage Change in New Test",
     xlab="Date",
     ylab="Percentage change of New Test",
     type="l",
     ylim = c(0,8.5),
     xlim = as.Date(c("2020-03-10","2021-08-25")),
     col="red") 













