##Econ 691 HW3
##Dr. Groves
###################################

library(rvest) 
library(dplyr)
library(tidyverse)
library(data.table)
library(readr)
library(ggplot2)
library(cowplot)
library(sf)

Perc<-function(x,y){
  temp<- ((y-x)/x)*100
  return(round(temp,4))
}


countypres_2000_2020 <- read_csv("Data/countypres_2000-2020nn.csv")

#########
load("./Build/Output/VOTES.RData")

srtvotes <- VOTES[order(state,County),]



D_Votes <- srtvotes


D_Votes$perdem <- Perc(srtvotes$Clinton,countypres_2000_2020$Biden)
D_Votes$perrep <- Perc(srtvotes$Trump,countypres_2000_2020$Trump)
D_Votes = select(D_Votes,-2:-5)


################Part 2###########
load("./Build/Output/Census2.RData")

Census2$NAME <- sub("County,.*", "", Census2$NAME)
Census2$NAME <- sub("Parish,.*", "", Census2$NAME)

names(Census2)[names(Census2) == "NAME"] <- "County"

CenVote <- merge(Census2,D_Votes,by.x=c("County","state"),by.y=c("County","state"),all = TRUE)
CenVote[is.na(CenVote)] = 0


view(CenVote)

#####################Part2 - 1 map

attach(CenVote)

P1 <- ggplot(CenVote)+
  geom_sf(aes(fill = perrep))+labs(title = "Percentage Change for Republican") +
  scale_fill_gradient(low="white",high="Red",limits=c(0,1),
                      aes(name="Percent Republican"))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank()) +
  geom_sf(
    CenVote = CenVote$County,
    fill= "red", colour="black",
    size=1,
    inherit.aes=FALSE
)


P2 <- ggplot(CenVote)+
  geom_sf(aes(fill = perdem))+ labs(title = "Percentage Change for Democrats")+
  scale_fill_gradient(low="white",high="blue",limits=c(0,1),
                      aes(name="Percent Democrat"))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank()) +
  geom_sf(
    CenVote = CenVote$County,
    fill="blue", colour="Black",
    size=1,
    inherit.aes=FALSE
  ) 

Maps <- plot_grid(P1,P2)


ggsave(Maps, file="PerChange.eps", device="eps")

##################PART 3 ###########################
load("./Build/Output/Census3.RData")

census3$NAME <- sub("County,.*", "", census3$NAME)
census3$NAME <- sub("Parish,.*", "", census3$NAME)

names(census3)[names(census3) == "NAME"] <- "County"


CenVote$geometry<-NULL
census3$geometry<-NULL

View(census3)
View(CenVote)



Core <- merge(CenVote,census3,by.x=c("County","state"),by.y=c("County","state"))

Core$perrep <- D_Votes$perrep
Core$perdem <- D_Votes$perdem



View(Core)

attach(Core)
mod1<-lm(perdem~perMale+perWhite)

mod2<-lm(perrep~perMale+perWhite)

mod3<-lm(perrep~chmale+chwhite)

mod4<-lm(perdem~chmale+chwhite)

mod5<-lm(perdem~chmale+chwhite+factor(state)-1)

mod6<-lm(perrep~chmale+chwhite+factor(state)-1)


library(stargazer)

stargazer(Core, type="html", out="./Build/Output/SumStat.html")

stargazer(mod1, mod2, mod3, mod4, type="html",
          out="./Build/Output/regress.html")

stargazer(mod2, type="html",
          out="./Build/Output/mod2perrep.html")

stargazer(mod1, type="html",out="./Build/Output/mod1perdem.html")



summary(mod1)
summary(mod2)

m1<-ggplot(mod1, aes(x=seq(1,503)))+
  geom_line(aes(y=mod1$residuals), color="blue")+
  geom_line(aes(y=0), color="black")+
  theme_light()
m1

m1 <-ggplot(Core, aes(x=perWhite))+
  geom_point(aes(y=perdem))+ xlim(0,1)+
  xlab("Percent Population White")+ ylim(0,100)+
  ylab("Percent repubican")+
  geom_line(aes(y=mod1$fitted.values), color="red")+
  theme_bw()
m1

with(Core,plot(perMale, perWhite))


plot(mod1)

plot(mod2)

plot(mod3)

plot(mod4)

plot(mod5)

plot(mod6)




