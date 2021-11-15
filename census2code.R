install.packages("tidycensus")

library(tidyverse)
library(tidycensus)

census_api_key("38138b77c559d52fa845c8992ef6f298a6f5412c",install = TRUE)


vars <- c("B01001_001","B01001_002","B02001_001","B02001_002", 
          "B02001_003","B05001_001","B05001_006","B07001_001", 
          "B07001_017","B07001_033","B07001_049", "B07001_065", "B07001_081")
          
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

Map2 <-rbindlist(list(tx.acs19,ak.acs19,LA.acs19,NM.acs19,OK.acs19), use.names = TRUE, fill=TRUE)
view(Map2)
Map2 = select(Map2,-2:-15,-17:-26)
save(Map2,file = "./Build/Output/Map2.RData")

Census2 <- rbindlist(list(tx.acs19,ak.acs19,LA.acs19,NM.acs19,OK.acs19), use.names = TRUE, fill=TRUE)
Census2 = select(Census2,-1,-3:-15)
view(Census2)
save(Census2,file = "./Build/Output/Census2.RData")
#######################census3
          