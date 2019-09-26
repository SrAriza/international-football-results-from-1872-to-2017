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

#----Loading the data----#
Data <- read.csv("results.csv")


#Checking the data 
summary(Data)

#Chaning data types
Data$date <- as.Date(Data$date)

#Creating new variable for total goals per game:
Data$TotalGoals <- Data$home_score + Data$away_score




# define relevant tournaments
rel_tour <- c("Copa AmÃ©rica","FIFA World Cup qualification", "FIFA World Cup",
              "UEFA Euro", "UEFA Euro qualification", "FIFA World Cup","Copa AmÃ©rica qualification", 
              "African Cup of Nations","AFC Asian Cup", "AFC Asian Cup qualification")

#Making a new variable for major tournaments
Data %>% 
  mutate(check = if_else(tournament %in% rel_tour, 1, 0)) %>% 
  select(check) -> Data$MajorTournament

Data$MajorTournament <- as.numeric(Data$MajorTournament)

#Visualitacion of goals scored over the years:
ggplot(Data, aes(x = date, y = TotalGoals, col = MajorTournament)) + geom_point()

