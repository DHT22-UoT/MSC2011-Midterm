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

### Data Cleaning: Trip Data ###

## Duration: Filter out trips that are less than 2 mins. These are most likely cancelled trips ##

  # Number of likely cancelled trips (2499 obs)
sum(trip$duration < 120)

  # Remove likely cancelled trips (left with 323840 obs)
trip1 <- trip %>%
  filter(duration >= 120)


## Duration: Identify & remove outliers ##

summary(trip1)
summary(trip1$duration) # Maximum duration: 17270400s (199.89 days)

hist(log10(trip1$duration))
boxplot(log10(trip1$duration))

  # Remove outliers based on IQR
trip1q <- quantile(trip1$duration)
trip1iqr <- IQR(trip1$duration) 

upperlimit <- trip1q[4] * 1.5
lowerlimit <- trip1q[2] * 1.5
lowerlimit <- trip1q[2] - 172.5

trip2 <- trip1 %>%
  filter(duration < upperlimit) %>%
  filter(duration > lowerlimit)
summary(trip2$duration)


## Stations: Filter out trips with invalid stations ##

  # Inconsistent spelling between the trip.csv & station.csv file
  # Ensure consistency by replacing all "Kearny" in trip2 with "Kearney"
trip2$start_station_name <- stringr::str_replace(trip2$start_station_name, "Kearny", "Kearney")
trip2$end_station_name <- stringr::str_replace(trip2$end_station_name, "Kearny", "Kearney")

  # Filter out trips where the start/end station name is not found in the station.csv
trip3 <- trip2 %>%
  filter(start_station_name %in% station$name) %>%
  filter(end_station_name %in% station$name)

  #' Observation: All excluded observations were trips to/from "Broadway at Main" 
  #' or "San Jose Government Center", which are not found in the station.csv file.
  #' (Note: the "San Jose Government Center" station is assumed different than the 
  #' "San Jose Civic Center" station)

##################################################################################

### Data Cleaning: Weather Data ###

## Conversion to factor ##

weather1 <- weather %>%
  # cloud_cover
  mutate(cloud_cover = as.factor(cloud_cover)) %>%
  
  # events (combined levels "rain" and "Rain")
  mutate(events = replace(events, events == "rain", "Rain")) %>%
  mutate(events = as.factor(events)) %>%

  # zip_code
  mutate(zip_code = as.factor(zip_code)) %>%
  
  # city
  mutate(city = as.factor(city))

#' Note from Danni: precipitation_inches is a numerical variable, but also contained "T" in some rows,
#' so it was treated as a categorical variable. Need to decide what to do with the "T"
  

## Dealing with NAs ##

which(is.na(weather1$max_visibility_miles))
which(is.na(weather1$mean_visibility_miles))
which(is.na(weather1$min_visibility_miles))

#' There are 9 observations that did not report max_visibility_miles, mean_visibility_miles, 
#' or min_visibility_miles, therefore, these 9 observations are removed. 

weather2 <- weather1 %>%
  filter(!is.na(max_visibility_miles))

# Note from Danni: remaining variables with NAs -> max_gust_speed_mph(443), events(1464)


## Remove outliers ##





