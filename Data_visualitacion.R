#######################################################################
#       international football results from 1872 to 2017              #
#                         by Julian Ariza                             #
#######################################################################


#----Libraries----#
library(dplyr)
library(ggplot2)
library(e1071)
library(tidyr)
library(readr)
library(tidyverse)

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



