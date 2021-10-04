#HW2 
#Econ 691 


rm(list=ls())

library(rvest) 
library(dplyr)
library(tidyverse)
library(data.table)


###############election 2016####################


states<-c("texas","new-mexico","oklahoma","arkansas","louisiana")

for(i in states){
  
  url.1 <- "https://www.nytimes.com/elections/2016/results/"
  
  url<-paste0(url.1,i)
  webpage <- read_html(url)
  tables<-webpage %>%
    html_nodes("table") #This pulls out all the "table" nodes in the HTML code
  
  results2<-tables[2] %>%
    html_table(fill=TRUE,header=TRUE) %>%
    as.data.frame() %>%
    rename("County" = "Vote.by.county") %>%
    mutate("Clinton" = as.numeric(gsub(",","",Clinton)), 
           "Trump" = as.numeric(gsub(",","",Trump)),
           "pctClinton" = (Clinton)/(Clinton+Trump),
           "pctTrump" = Trump/(Clinton+Trump)) 
  assign(i,results2)
}
view(results2)


texas$state <- "texas"
arkansas$state <-"arkansas"
louisiana$state <- "louisiana"
`new-mexico`$state <- "new-mexico"
oklahoma$state<- "oklahoma"

#I created a single data frame that holds all of the votes from the tables

VOTES <- rbindlist(list(arkansas,louisiana,`new-mexico`,oklahoma,texas), use.names = TRUE, fill=TRUE) 


view(VOTES)



#######################2016 acs##############################################
rm(list=ls())
install.packages("tidycensus")

library(tidyverse)
library(tidycensus)

census_api_key("38138b77c559d52fa845c8992ef6f298a6f5412c",install = TRUE)


vars <- c("B01001_001","B01001_002","B02001_001","B02001_002", 
          "B02001_003","B05001_001","B05001_006","B07001_001", 
          "B07001_017","B07001_033","B07001_049", "B07001_065", "B07001_081")

#acs for 2016

texasacs16 <- get_acs(geography = "county", 
               variables = vars,
               state = 48,
               year = 2016,
               geometry = TRUE) 

arkansasacs16 <- get_acs(geography = "county", 
               variables = vars,
               state = 05,
               year = 2016,
               geometry = TRUE) 

louisianaacs16 <- get_acs(geography = "county", 
               variables = vars,
               state = 22,
               year = 2016,
               geometry = TRUE) 

newmexicoacs16 <- get_acs(geography = "county", 
               variables = vars,
               state = 35,
               year = 2016,
               geometry = TRUE)

oklahomaacs16 <- get_acs(geography = "county", 
               variables = vars,
               state = 40,
               year = 2016,
               geometry = TRUE) 



#i change the names to their proper forms
tx.acs16<-texasacs16 %>%
  mutate(variable2 = case_when(variable=="B01001_001" ~ "TotPop",
                               variable=="B01001_002" ~ "Male",
                               variable=="B02001_001" ~ "TotRace",
                               variable=="B02001_002" ~ "White", 
                               variable=="B02001_003" ~ "Black",
                               variable=="B05001_001" ~ "TotCit",
                               variable=="B05001_006" ~ "NonCit",
                               variable=="B07001_001" ~ "TotMob",
                               variable=="B07001_017" ~ "Stay",
                               variable=="B07001_033" ~ "SameCounty", 
                               variable=="B07001_049" ~ "SameSt",
                               variable=="B07001_065" ~ "OthState",
                               variable=="B07001_081" ~ "Abroad",
                               TRUE ~ "other")) %>%
  select(!c(moe,variable)) %>%
  spread(key=variable2, value=estimate)


ak.acs16<-arkansasacs16 %>%
  mutate(variable2 = case_when(variable=="B01001_001" ~ "TotPop",
                               variable=="B01001_002" ~ "Male",
                               variable=="B02001_001" ~ "TotRace",
                               variable=="B02001_002" ~ "White", 
                               variable=="B02001_003" ~ "Black",
                               variable=="B05001_001" ~ "TotCit",
                               variable=="B05001_006" ~ "NonCit",
                               variable=="B07001_001" ~ "TotMob",
                               variable=="B07001_017" ~ "Stay",
                               variable=="B07001_033" ~ "SameCounty", 
                               variable=="B07001_049" ~ "SameSt",
                               variable=="B07001_065" ~ "OthState",
                               variable=="B07001_081" ~ "Abroad",
                               TRUE ~ "other")) %>%
  select(!c(moe,variable)) %>%
  spread(key=variable2, value=estimate)





LA.acs16<-louisianaacs16 %>%
  mutate(variable2 = case_when(variable=="B01001_001" ~ "TotPop",
                               variable=="B01001_002" ~ "Male",
                               variable=="B02001_001" ~ "TotRace",
                               variable=="B02001_002" ~ "White", 
                               variable=="B02001_003" ~ "Black",
                               variable=="B05001_001" ~ "TotCit",
                               variable=="B05001_006" ~ "NonCit",
                               variable=="B07001_001" ~ "TotMob",
                               variable=="B07001_017" ~ "Stay",
                               variable=="B07001_033" ~ "SameCounty", 
                               variable=="B07001_049" ~ "SameSt",
                               variable=="B07001_065" ~ "OthState",
                               variable=="B07001_081" ~ "Abroad",
                               TRUE ~ "other")) %>%
  select(!c(moe,variable)) %>%
  spread(key=variable2, value=estimate)




NM.acs16<-newmexicoacs16 %>%
  mutate(variable2 = case_when(variable=="B01001_001" ~ "TotPop",
                               variable=="B01001_002" ~ "Male",
                               variable=="B02001_001" ~ "TotRace",
                               variable=="B02001_002" ~ "White", 
                               variable=="B02001_003" ~ "Black",
                               variable=="B05001_001" ~ "TotCit",
                               variable=="B05001_006" ~ "NonCit",
                               variable=="B07001_001" ~ "TotMob",
                               variable=="B07001_017" ~ "Stay",
                               variable=="B07001_033" ~ "SameCounty", 
                               variable=="B07001_049" ~ "SameSt",
                               variable=="B07001_065" ~ "OthState",
                               variable=="B07001_081" ~ "Abroad",
                               TRUE ~ "other")) %>%
  select(!c(moe,variable)) %>%
  spread(key=variable2, value=estimate)



OK.acs16<-oklahomaacs16 %>%
  mutate(variable2 = case_when(variable=="B01001_001" ~ "TotPop",
                               variable=="B01001_002" ~ "Male",
                               variable=="B02001_001" ~ "TotRace",
                               variable=="B02001_002" ~ "White", 
                               variable=="B02001_003" ~ "Black",
                               variable=="B05001_001" ~ "TotCit",
                               variable=="B05001_006" ~ "NonCit",
                               variable=="B07001_001" ~ "TotMob",
                               variable=="B07001_017" ~ "Stay",
                               variable=="B07001_033" ~ "SameCounty", 
                               variable=="B07001_049" ~ "SameSt",
                               variable=="B07001_065" ~ "OthState",
                               variable=="B07001_081" ~ "Abroad",
                               TRUE ~ "other")) %>%
  select(!c(moe,variable)) %>%
  spread(key=variable2, value=estimate)









#I could not go through with the process of mutating as my r was bringing up errors so i just 
# did it manually

{attach(tx.acs16)
tx.acs16$perMale = tx.acs16$Male/(tx.acs16$TotPop)
tx.acs16$perWhite = White/(tx.acs16$TotPop)
tx.acs16$perBlack = Black/(tx.acs16$TotPop)
tx.acs16$perCit = 1-(NonCit/TotCit)
tx.acs16$perStay = Stay/TotMob
tx.acs16$perSameCounty = SameCounty/TotMob
tx.acs16$perSameSt = SameSt/TotMob
tx.acs16$perOthState = OthState/TotMob 
tx.acs16$perAbroad = Abroad/TotMob
  }

{attach(ak.acs16)
ak.acs16$perMale = Male/(ak.acs16$TotPop)
ak.acs16$perWhite = White/(ak.acs16$TotPop)
ak.acs16$perBlack = Black/(ak.acs16$TotPop)
ak.acs16$perCit = 1-(NonCit/TotCit)
ak.acs16$perStay = Stay/TotMob
ak.acs16$perSameCounty = SameCounty/TotMob
ak.acs16$perSameSt = SameSt/TotMob
ak.acs16$perOthState = OthState/TotMob 
ak.acs16$perAbroad = Abroad/TotMob
}

{attach(LA.acs16)
LA.acs16$perMale = Male/(LA.acs16$TotPop)
LA.acs16$perWhite = White/(LA.acs16$TotPop)
LA.acs16$perBlack = Black/(LA.acs16$TotPop)
LA.acs16$perCit = 1-(NonCit/TotCit)
LA.acs16$perStay = Stay/TotMob
LA.acs16$perSameCounty = SameCounty/TotMob
LA.acs16$perSameSt = SameSt/TotMob
LA.acs16$perOthState = OthState/TotMob 
LA.acs16$perAbroad = Abroad/TotMob
}

{attach(NM.acs16)
NM.acs16$perMale = Male/(NM.acs16$TotPop)
NM.acs16$perWhite = White/(NM.acs16$TotPop)
NM.acs16$perBlack = Black/(NM.acs16$TotPop)
NM.acs16$perCit = 1-(NonCit/TotCit)
NM.acs16$perStay = Stay/TotMob
NM.acs16$perSameCounty = SameCounty/TotMob
NM.acs16$perSameSt = SameSt/TotMob
NM.acs16$perOthState = OthState/TotMob 
NM.acs16$perAbroad = Abroad/TotMob
}

{attach(OK.acs16)
OK.acs16$perMale = Male/(OK.acs16$TotPop)
OK.acs16$perWhite = White/(OK.acs16$TotPop)
OK.acs16$perBlack = Black/(OK.acs16$TotPop)
OK.acs16$perCit = 1-(NonCit/TotCit)
OK.acs16$perStay = Stay/TotMob
OK.acs16$perSameCounty = SameCounty/TotMob
OK.acs16$perSameSt = SameSt/TotMob
OK.acs16$perOthState = OthState/TotMob 
OK.acs16$perAbroad = Abroad/TotMob
}


tx.acs16$state <- "texas"
ak.acs16$state <-"arkansas"
LA.acs16$state <- "louisiana"
NM.acs16$state <- "new-mexico"
OK.acs16$state<- "oklahoma"



Census1 <- rbindlist(list(tx.acs16,ak.acs16,LA.acs16,NM.acs16,OK.acs16), use.names = TRUE, fill=TRUE)
Census1 = select(Census1,-1,-3:-15)
view(Census1)



#######################2019acs########################################

#acs for 2019

load_variables(2019,"acs5")

texasacs19 <- get_acs(geography = "county",
                      variables = vars,
                      state = 48,
                      year = 2019,
                      geometry = TRUE) 

arkansasacs19 <- get_acs(geography = "county", 
                         variables = vars,
                         state = 05,
                         year = 2019,
                         geometry = TRUE) 

louisianaacs19 <- get_acs(geography = "county", 
                          variables = vars,
                          state = 22,
                          year = 2019,
                          geometry = TRUE) 

newmexicoacs19 <- get_acs(geography = "county", 
                           variables = vars,
                           state = 35,
                           year = 2019,
                           geometry = TRUE)

oklahomaacs19 <- get_acs(geography = "county", 
                         variables = vars,
                         state = 40,
                         year = 2019,
                         geometry = TRUE) 



#i change the names to their proper forms
tx.acs19<-texasacs19 %>%
  mutate(variable2 = case_when(variable=="B01001_001" ~ "TotPop",
                               variable=="B01001_002" ~ "Male",
                               variable=="B02001_001" ~ "TotRace",
                               variable=="B02001_002" ~ "White", 
                               variable=="B02001_003" ~ "Black",
                               variable=="B05001_001" ~ "TotCit",
                               variable=="B05001_006" ~ "NonCit",
                               variable=="B07001_001" ~ "TotMob",
                               variable=="B07001_017" ~ "Stay",
                               variable=="B07001_033" ~ "SameCounty", 
                               variable=="B07001_049" ~ "SameSt",
                               variable=="B07001_065" ~ "OthState",
                               variable=="B07001_081" ~ "Abroad",
                               TRUE ~ "other")) %>%
  select(!c(moe,variable)) %>%
  spread(key=variable2, value=estimate)


ak.acs19<-arkansasacs19 %>%
  mutate(variable2 = case_when(variable=="B01001_001" ~ "TotPop",
                               variable=="B01001_002" ~ "Male",
                               variable=="B02001_001" ~ "TotRace",
                               variable=="B02001_002" ~ "White", 
                               variable=="B02001_003" ~ "Black",
                               variable=="B05001_001" ~ "TotCit",
                               variable=="B05001_006" ~ "NonCit",
                               variable=="B07001_001" ~ "TotMob",
                               variable=="B07001_017" ~ "Stay",
                               variable=="B07001_033" ~ "SameCounty", 
                               variable=="B07001_049" ~ "SameSt",
                               variable=="B07001_065" ~ "OthState",
                               variable=="B07001_081" ~ "Abroad",
                               TRUE ~ "other")) %>%
  select(!c(moe,variable)) %>%
  spread(key=variable2, value=estimate)





LA.acs19<-louisianaacs19 %>%
  mutate(variable2 = case_when(variable=="B01001_001" ~ "TotPop",
                               variable=="B01001_002" ~ "Male",
                               variable=="B02001_001" ~ "TotRace",
                               variable=="B02001_002" ~ "White", 
                               variable=="B02001_003" ~ "Black",
                               variable=="B05001_001" ~ "TotCit",
                               variable=="B05001_006" ~ "NonCit",
                               variable=="B07001_001" ~ "TotMob",
                               variable=="B07001_017" ~ "Stay",
                               variable=="B07001_033" ~ "SameCounty", 
                               variable=="B07001_049" ~ "SameSt",
                               variable=="B07001_065" ~ "OthState",
                               variable=="B07001_081" ~ "Abroad",
                               TRUE ~ "other")) %>%
  select(!c(moe,variable)) %>%
  spread(key=variable2, value=estimate)




NM.acs19<-newmexicoacs19 %>%
  mutate(variable2 = case_when(variable=="B01001_001" ~ "TotPop",
                               variable=="B01001_002" ~ "Male",
                               variable=="B02001_001" ~ "TotRace",
                               variable=="B02001_002" ~ "White", 
                               variable=="B02001_003" ~ "Black",
                               variable=="B05001_001" ~ "TotCit",
                               variable=="B05001_006" ~ "NonCit",
                               variable=="B07001_001" ~ "TotMob",
                               variable=="B07001_017" ~ "Stay",
                               variable=="B07001_033" ~ "SameCounty", 
                               variable=="B07001_049" ~ "SameSt",
                               variable=="B07001_065" ~ "OthState",
                               variable=="B07001_081" ~ "Abroad",
                               TRUE ~ "other")) %>%
  select(!c(moe,variable)) %>%
  spread(key=variable2, value=estimate)



OK.acs19<-oklahomaacs19 %>%
  mutate(variable2 = case_when(variable=="B01001_001" ~ "TotPop",
                               variable=="B01001_002" ~ "Male",
                               variable=="B02001_001" ~ "TotRace",
                               variable=="B02001_002" ~ "White", 
                               variable=="B02001_003" ~ "Black",
                               variable=="B05001_001" ~ "TotCit",
                               variable=="B05001_006" ~ "NonCit",
                               variable=="B07001_001" ~ "TotMob",
                               variable=="B07001_017" ~ "Stay",
                               variable=="B07001_033" ~ "SameCounty", 
                               variable=="B07001_049" ~ "SameSt",
                               variable=="B07001_065" ~ "OthState",
                               variable=="B07001_081" ~ "Abroad",
                               TRUE ~ "other")) %>%
  select(!c(moe,variable)) %>%
  spread(key=variable2, value=estimate)









#I could not go through with the process of mutating as my r was bringing up errors so i just 
# did it manually
{attach(tx.acs19)
tx.acs19$perMale = Male/(tx.acs19$TotPop)
tx.acs19$perWhite = White/(tx.acs19$TotPop)
tx.acs19$perBlack = Black/(tx.acs19$TotPop)
tx.acs19$perCit = 1-(NonCit/TotCit)
tx.acs19$perStay = Stay/TotMob
tx.acs19$perSameCounty = SameCounty/TotMob
tx.acs19$perSameSt = SameSt/TotMob
tx.acs19$perOthState = OthState/TotMob 
tx.acs19$perAbroad = Abroad/TotMob
}

{attach(ak.acs19)
ak.acs19$perMale = Male/(ak.acs19$TotPop)
ak.acs19$perWhite = White/(ak.acs19$TotPop)
ak.acs19$perBlack = Black/(ak.acs19$TotPop)
ak.acs19$perCit = 1-(NonCit/TotCit)
ak.acs19$perStay = Stay/TotMob
ak.acs19$perSameCounty = SameCounty/TotMob
ak.acs19$perSameSt = SameSt/TotMob
ak.acs19$perOthState = OthState/TotMob 
ak.acs19$perAbroad = Abroad/TotMob
}

{attach(LA.acs19)
LA.acs19$perMale = Male/(LA.acs19$TotPop)
LA.acs19$perWhite = White/(LA.acs19$TotPop)
LA.acs19$perBlack = Black/(LA.acs19$TotPop)
LA.acs19$perCit = 1-(NonCit/TotCit)
LA.acs19$perStay = Stay/TotMob
LA.acs19$perSameCounty = SameCounty/TotMob
LA.acs19$perSameSt = SameSt/TotMob
LA.acs19$perOthState = OthState/TotMob 
LA.acs19$perAbroad = Abroad/TotMob
}

{attach(NM.acs19)
NM.acs19$perMale = Male/(NM.acs19$TotPop)
NM.acs19$perWhite = White/(NM.acs19$TotPop)
NM.acs19$perBlack = Black/(NM.acs19$TotPop)
NM.acs19$perCit = 1-(NonCit/TotCit)
NM.acs19$perStay = Stay/TotMob
NM.acs19$perSameCounty = SameCounty/TotMob
NM.acs19$perSameSt = SameSt/TotMob
NM.acs19$perOthState = OthState/TotMob 
NM.acs19$perAbroad = Abroad/TotMob
}

{attach(OK.acs19)
OK.acs19$perMale = Male/(OK.acs19$TotPop)
OK.acs19$perWhite = White/(OK.acs19$TotPop)
OK.acs19$perBlack = Black/(OK.acs19$TotPop)
OK.acs19$perCit = 1-(NonCit/TotCit)
OK.acs19$perStay = Stay/TotMob
OK.acs19$perSameCounty = SameCounty/TotMob
OK.acs19$perSameSt = SameSt/TotMob
OK.acs19$perOthState = OthState/TotMob 
OK.acs19$perAbroad = Abroad/TotMob
}

tx.acs19$state <- "texas"
ak.acs19$state <-"arkansas"
LA.acs19$state <- "louisiana"
NM.acs19$state <- "new-mexico"
OK.acs19$state<- "oklahoma"



Census2 <- rbindlist(list(tx.acs19,ak.acs19,LA.acs19,NM.acs19,OK.acs19), use.names = TRUE, fill=TRUE)
Census2 = select(Census2,-1,-3:-15)
view(Census2)

#######################census3###########

Perc<-function(x,y){
  temp<- ((y-x)/x)
  return(round(temp,4))
}

census3 <- Census1
census3 =select(census3,-3:-11,-13)
view(census3)



census3$chmale <- Perc(Census1$perMale,Census2$perMale)
census3$chblack <- Perc(Census1$perBlack,Census2$perBlack)
census3$chwhite <- Perc(Census1$perWhite,Census2$perWhite)
census3$chabroud <- Perc(Census1$perAbroad,Census2$perAbroad)
census3$chcit <- Perc(Census1$perCit,Census2$perCit)
census3$chstay <- Perc(Census1$perStay,Census2$perStay)
census3$chsamecounty <- Perc(Census1$perSameCounty,Census2$perSameCounty)
census3$chsmstate <- Perc(Census1$perSameSt,Census2$perSameSt)
census3$chothst <- Perc(Census1$perOthState,Census2$perOthState)




view(census3)


############################plots###########################################

library(ggplot2)
library(cowplot)

attach(VOTES)

view(tx.acs16)


ggplot(tx.acs16) +
  geom_sf(aes(fill = perWhite))

core<-merge(tx.acs16,texas,by.x="state",by.y="state")

p1<-ggplot(core)+
  geom_sf(aes(fill=pctClinton))+
  scale_fill_gradient(low="white",high="blue",limits=c(0,1),aes(name="Percent Clinton"))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank()) 


p2<-ggplot(core)+
  geom_sf(aes(fill=perWhite))+
  scale_fill_gradient(low="black",high="white",limits=c(0,1),aes(name="Percent White"))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank()) 

plot_grid(p1,p2)

plot_grid(p1)

plot_grid(p2)


