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

EurAmer <- c("Europe", "Americas")

WorldCup %>% 
  group_by(year) %>% 
  filter(region == EurAmer) -> WorldCup1

WorldCup1$result <- as.factor(WorldCup1$result)

WorldCup1 %>% 
  summarise(frequency= n() )) %>% 
  ggplot(aes(x = year , y = AllResults )) + geom_line()
