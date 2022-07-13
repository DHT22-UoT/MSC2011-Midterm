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

  # Remove outliers based on IQR (Q3 + 1.5 * IQR or Q1 - 1.5 * IQR)
trip1q <- quantile(trip1$duration) # Q1 = 345; Q3 = 748
trip1iqr <- IQR(trip1$duration) # IQR = 403

upperlimit <- trip1q[4] + 1.5*trip1iqr
lowerlimit <- trip1q[2] - 1.5*trip1iqr

trip2 <- trip1 %>%
  filter(duration < upperlimit) %>%
  filter(duration > lowerlimit)
summary(trip2$duration)


## Stations: Filter out trips with invalid stations ##

  # Inconsistent spelling between the trip.csv & station.csv file
  # Ensure consistency by replacing all "Kearny" in trip2 with "Kearney"
trip2$start_station_name <- stringr::str_replace(trip2$start_station_name, "Kearny", "Kearney")
trip2$end_station_name <- stringr::str_replace(trip2$end_station_name, "Kearny", "Kearney")

  # Inconsistency due to duplication 
length(unique(trip2$start_station_id)) # 70 unique start station ids
length(unique(trip2$start_station_name)) # 72 unique start station names

  # Filter out trips where the start/end station name is not found in the station.csv
trip3 <- trip2 %>%
  filter(start_station_name %in% station$name) %>%
  filter(end_station_name %in% station$name) %>%
  
  # Filter out trips where the start/end station id is not found in the station.csv
  filter(start_station_id %in% station$id) %>%
  filter(end_station_id %in% station$id)

  #' Observation: All excluded observations were trips to/from "Broadway at Main" 
  #' or "San Jose Government Center", which are not found in the station.csv file.
  #' In the trip dataset, both the "San Jose Government Center" station and the 
  #' "Santa Clara County Civic Center" station has a station id of 80. To be 
  #' consistent with the station dataset, station id 80 corresponds to the 
  #' "Santa Clara County Civic Center" station, and observations with "San Jose 
  #' Government Center" station are removed
table(trip2$start_station_name[trip2$start_station_id == "80"])

##################################################################################

### Rush Hours Task ###
  
  # Add rush hours to the dataset; find the hours of weekdays where the trip volume is highest

## Set-up ##
library(lubridate)
install.packages("tidyr")


## Data tranformation ##
trip4 <- trip3 %>%
  # Creating new variable trip_day
  mutate(start_date = as.POSIXct(start_date, format="%m/%d/%Y%H:%M")) %>%
  mutate(trip_day = wday(start_date, label=TRUE, abbr=FALSE)) %>%
  
  # Subsetting the weekdays 
  filter(trip_day != "Saturday") %>%
  filter(trip_day != "Sunday") %>%
  
  # Extract time from start_date
  mutate(start_hour = hour(start_date))

  # Shows the counts of weekdays
dplyr::count(trip4, trip4$trip_day)


## GGPlots showing highest trip volumn ##


