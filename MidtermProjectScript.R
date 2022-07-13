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
library(lubridate)

## Reading cleaned csv files into dataframe
trip_clean <- read.csv("trip_clean.csv")
station <- read.csv("station.csv")
weather_clean <- read.csv("weather_clean.csv")

##################################################################################

### Tables to be joined ###
trip5 <- trip_clean %>%
  mutate(start_date = as.POSIXct(start_date, format="%m/%d/%Y%H:%M")) %>%
  mutate(trip_day = wday(start_date, label=TRUE, abbr=FALSE))

weather_clean
station

### Joining tables ###

# Joining station & trip5 by station id (to get city)
stationtrip_join <- inner_join(station, trip5, by = c("id" = "start_station_id"))

  # Create a new variable to extract date from start_date in stationtrip_join 
stationtrip_join <- stationtrip_join %>%
  mutate(date = as.Date(start_date))
  
  # Convert variable "date" in weather_clean to class date for consistency
weather_clean <- weather_clean %>%
  mutate(date = as.Date(date))

# Joining stationtrip_join & weather_clean by date and city
weather_join <- inner_join(stationtrip_join, weather_clean, by = c("date" = "date", "city" = "city"))

# Select weather variables from the joined table
weather_join1 <- weather_join %>%
  select(c(duration, max_temperature_f:precipitation_inches))


### Correlation ###
install.packages("corrplot")
library("corrplot")

cor_matrix <- cor(weather_join1, use="complete.obs")
corrplot(cor_matrix, method = "circle")


