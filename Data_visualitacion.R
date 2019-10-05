#######################################################################
#       international football results from 1872 to 2017              #
#                         by Julian Ariza                             #
#######################################################################


#----Libraries----#

library(e1071)
library(tidyverse)
library(lubridate)

#----Loading the data----#
Data <- read.csv("results.csv")
Countries <- read_csv("countries.csv")
errors <- read_csv2("errors (2).csv")

#Checking the data 
summary(Data)

#Chaning data types
Data$date <- as.Date(Data$date)

#Creating new variable for total goals per game:
Data$TotalGoals <- Data$home_score + Data$away_score




# define relevant tournaments
rel_tour <- c("Copa AmÃ©rica","FIFA World Cup qualification", "FIFA World Cup",
              "UEFA Euro", "UEFA Euro qualification", "FIFA World Cup","Copa AmÃ©rica qualification")

#Making a new variable for major tournaments
Data %>% 
  mutate(MajorTournament = if_else(tournament %in% rel_tour, "True", "False")) -> Data

Data$MajorTournament <-  as.factor(Data$MajorTournament)

#Visualitacion of goals scored over the years:
ggplot(Data, aes(x = date, y = TotalGoals, col = MajorTournament)) + geom_bar()


#Creating a new variable to find out who won the game
Data$GoalsDifference <- Data$home_score - Data$away_score

#New dataset only for major tournaments
Data %>% 
  filter(MajorTournament == "True" ) -> MajorTournaments



#Adding the continent to the dataset
#Creating the dataset only with country name and region
Countries %>% 
  select(home_team = name, region) -> SelectedCountries

#adding the countries with different names or errors encountered
ReadyData <- bind_rows(SelectedCountries, errors)

#Joining the dataset to add region to the dataset
MajorTournaments %>% 
  left_join(y = ReadyData, by = "home_team") -> AllCountries

#How i found the errors the first time
AllCountries %>% 
  filter(is.na(region)) %>% 
  distinct(home_team) -> errors

#write.csv(errors, file = "errors.csv")

#Checking with the new dataset for errors

AllCountries %>% 
  filter(is.na(region)) %>% 
  distinct(home_team)


#Creating a region for away team
ReadyData %>% 
  rename( away_team = home_team, region2= region) -> AwayTeam

#Creating a dataset with regions for home and away team
AllCountries %>% 
  left_join(y = AwayTeam, by = "away_team") -> HomeAndAway

#Visualitacion of goals scored over the years by continent:
HomeAndAway
ggplot(Data, aes(x = date, y = TotalGoals, col = MajorTournament)) + geom_bar()


#Checking for who won:
#Creating a different data set for home win, draw and away win.
HomeAndAway %>% 
  filter(GoalsDifference >0) -> HomeWin

HomeAndAway %>% 
  filter(GoalsDifference ==0) -> Draw

HomeAndAway %>% 
  filter(GoalsDifference <0) -> AwayWin

#Creating a new variable called result where it would reflect what was the score of the match
mutate(HomeWin, result = "H") ->HomeWin

mutate(AwayWin, result = "A") ->AwayWin

mutate(Draw, result = "D") ->Draw

#Putting together all the results with the diffent outcomes
bind_rows(HomeWin, AwayWin, Draw)->Results

#Reordering the results so they are ordered by date.
ResultsOrder <- order(Results$date)
WorldCup <- Results[ResultsOrder,]



#Checking the outcome when american teams play european at home at the world cup
WorldCup %>% 
  filter(region == "Americas") %>% 
  filter(region2 == "Europe") -> AmericaHome


ggplot(AmericaHome, aes(x = result))+ geom_bar(aes(fill= result))

#Checking the outcome when european teams play american teams at home at the world cup
WorldCup %>% 
  filter(region == "Europe") %>% 
  filter(region2 == "Americas") -> EuropeHome


ggplot(EuropeHome, aes(x = result))+ geom_bar(aes(fill= result))


EuropeHome %>% 
  group_by(date, "year" ) %>% 
  group_by(result) -> test

ggplot(EuropeHome, aes(x = date, y = result))+ geom_line()



EuropeHome$date <- as.POSIXct(EuropeHome$date, "%Y/%m/%d")

EuropeHome$year <- year(EuropeHome$date)

#Creating a ggplot for the results between europe and south america in the world cup
WorldCup$date <- as.POSIXct(WorldCup$date, "%Y/%m/%d")

WorldCup$year <- year(WorldCup$date)


#Creating a dataset with only world cup games
WorldCup %>% 
  filter(tournament == "FIFA World Cup") -> WorldCup1

#Creating a dataset only with the 2018 world cup
WorldCup1 %>% 
  filter(year == 2018) %>% 
  arrange(desc(date)) -> WorldCup2018

#Creating an id for all the games
id <- c(1:64)

WorldCup2018$id <- id

WorldCup2018$id <- as.numeric(WorldCup2018$id)

semifinal <- c(3,4)
quarterfinal <- c(5:8)
roundof16 <- c(9:16)
GroupStage <- c(17:64)

for(i in 1:nrow(WorldCup2018)){
  if(WorldCup2018$id[i] == 1){
    WorldCup2018$Phase[i] <- "Final"
  }else if (WorldCup2018$id[i] == 2){
    WorldCup2018$Phase[i] <- "3rd4thplace"
  }else if (WorldCup2018$id[i] %in% semifinal ){
    WorldCup2018$Phase[i] <- "Semifinal"
  }else if (WorldCup2018$id[i] %in% quarterfinal){
    WorldCup2018$Phase[i] <- "Quarterfinal"
  }else if (WorldCup2018$id[i] %in% roundof16){
    WorldCup2018$Phase[i] <- "RoundOf16"
    }else if (WorldCup2018$id[i] %in% GroupStage){
      WorldCup2018$Phase[i] <- "GroupStage"
    }
}

      
#Creating a new dataset with only the world cups with 32 teams
WorldCup1 %>% 
  filter(year > 1997) %>% 
  arrange(desc(date)) -> worldcup32teams 


worldcup32teams$id <- id

#Adding the world cup phase 
for(i in 1:nrow(worldcup32teams)){
  if(worldcup32teams$id[i] == 1){
    worldcup32teams$Phase[i] <- "Final"
  }else if (worldcup32teams$id[i] == 2){
    worldcup32teams$Phase[i] <- "3rd4thplace"
  }else if (worldcup32teams$id[i] %in% semifinal ){
    worldcup32teams$Phase[i] <- "Semifinal"
  }else if (worldcup32teams$id[i] %in% quarterfinal){
    worldcup32teams$Phase[i] <- "Quarterfinal"
  }else if (worldcup32teams$id[i] %in% roundof16){
    worldcup32teams$Phase[i] <- "RoundOf16"
  }else if (worldcup32teams$id[i] %in% GroupStage){
    worldcup32teams$Phase[i] <- "GroupStage"
  }
}


