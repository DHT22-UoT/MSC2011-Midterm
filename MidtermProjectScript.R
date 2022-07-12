#' MSC2011 Midterm Project
#' Muhammad Musa, Danni Ma

# Downloading and calling relevant packages
install.packages("tidyverse")
install.packages("funModeling")
install.packages("Hmisc")
install.packages("quantmod")
library(funModeling) 
library(tidyverse) 
library(Hmisc)
library("dplyr")

trip <- read.csv("trip.csv")
trip
station <- read.csv("station.csv")

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

  # Note from Danni: Not sure if "San Jose Civic Center" is the same as "San Jose Government Center", assumed different for now

# Filter out trips where the start/end station name is not found in the station.csv
trip3 <- trip2 %>%
  filter(start_station_name %in% station$name) %>%
  filter(end_station_name %in% station$name)
  

##################################################################################

### Rush Hours Task ###

library(lubridate)
install.packages("tidyr")
# adding rush hours to the dataset - highest volume of hours during weekdays
# I tried to first make a column that would say the day of each date - like monday, tues, wed, ...
# From this i was going to filter the weekdays only and find rush hours but for some reason only half the dataset shows the weekdays and the buttom half just say NA
# let me know if this makes sense and if you know what I did wrong.

## Create new variable trip_day ##
trip4 <- trip3 %>%
  mutate(start_date = as.POSIXct(start_date, format="%m/%d/%Y%H:%M")) %>%
  mutate(trip_day = wday(start_date, label=TRUE, abbr=FALSE))

dplyr::count(trip4, trip4$trip_day)

## Subsetting the weekdays in order to find rush hours

trip5 <- trip4 %>%
  filter(trip_day == c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))

dplyr::count(trip5, trip5$trip_day)

## In order to make a histogram, I think we need to split the start date column into date and time and then only use time to find rush hours


trip6 <- trip5 %>%
  mutate(starthour = hour(start_date)) %>%
  filter(trip_day == "Monday")
hist(trip6$starthour)
