#' MSC2011 Midterm Project
#' Muhammad Musa, Danni Ma

### Set-up ###

## Downloading and calling relevant packages
install.packages("tidyverse")
install.packages("funModeling")
install.packages("Hmisc")
install.packages("quantmod")
library(funModeling) 
library(tidyverse) 
library(Hmisc)
library("dplyr")

## Reading csv files into dataframe
trip <- read.csv("trip.csv", na.strings = "")
station <- read.csv("station.csv")
weather <- read.csv("weather.csv", na.strings = "")

##################################################################################

### Tables to be joined ###
trip5 
weather4
station

### Joining table ###

# Joining station & trip5 by station id (to get city)
stationtrip_join <- inner_join(station, trip5, by = c("id" = "start_station_id"))

# Joining stationtrip_joined & weather4 by date and city
weather_join <- inner_join(stationtrip_joined, weather4, by = c("start_date" = "date", "city" = "city"))

# Select weather variables from the joined table
weather_join1 <- weather_join %>%
  select(c(duration, max_temperature_f:precipitation_inches))


### Correlation ###
install.packages("corrplot")
library("corrplot")
corrplot(cor(weather_join1), method = "circle")


