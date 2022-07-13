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

### Data Cleaning: Weather Data ###

## Variable conversion ##

weather1 <- weather %>%
  # cloud_cover
  mutate(cloud_cover = as.factor(cloud_cover)) %>%
  
  # events (changed NA to "No event"; combined levels "rain" and "Rain")
  mutate(events = replace(events, is.na(events), "No event")) %>%
  mutate(events = replace(events, events == "rain", "Rain")) %>%
  mutate(events = as.factor(events)) %>%

  # zip_code
  mutate(zip_code = as.factor(zip_code)) %>%
  
  # city
  mutate(city = as.factor(city)) %>%
  
  # precipitation_inches ("T" is assumed to be trace amount, therefore is converted to 0)
  mutate(precipitation_inches = replace(precipitation_inches, precipitation_inches == "T", 0)) %>%
  mutate(precipitation_inches = as.numeric(as.character(precipitation_inches))) %>%

  # data
  mutate(date = as.POSIXct(date, format="%m/%d/%Y"))


## Fixing Variable Name ##

  # Rename the variable max_wind_Speed_mph to be consistent with the remaining variable 
weather2 <- weather1 %>%
  rename("max_wind_speed_mph" = "max_wind_Speed_mph")


## Dealing with NAs ##
which(is.na(weather1$max_visibility_miles))
which(is.na(weather1$mean_visibility_miles))
which(is.na(weather1$min_visibility_miles))

  #' There are 9 observations that did not report max_visibility_miles, mean_visibility_miles, 
  #' or min_visibility_miles, therefore, these 9 observations are removed. 

weather3 <- weather2 %>%
  filter(!is.na(max_visibility_miles))

## Remove outliers ##

  # max_temperature_f
outlier1 <- boxplot(weather3$max_temperature_f)$out
  # min_temperature_f
outlier2 <- boxplot(weather3$min_temperature_f)$out 
  # max_wind_speed_mph
outlier3 <- boxplot(weather3$max_wind_speed_mph)$out
  # mean_wind_speed_mph
outlier4 <- boxplot(weather3$mean_wind_speed_mph)$out
  # max_gust_speed_mph
outlier5 <- boxplot(weather3$max_gust_speed_mph)$out

# No outliers are removed for the following variables:

  # mean_temperature_f: no outliers observed
boxplot(weather3$mean_temperature_f)$out
  # max_visibility_miles: IQR = 0
boxplot(weather3$max_visibility_miles)$out
IQR(weather3$max_visibility_miles, na.rm=T)
  # mean_visibility_miles: IQR = 0
boxplot(weather3$mean_visibility_miles)$out
IQR(weather3$mean_visibility_miles, na.rm=T)
  # min_visibility_miles
boxplot(weather3$min_visibility_miles)$out
  # precipitation_inches
boxplot(weather3$precipitation_inches)$out

weather4 <- weather3 %>%
  filter(!(max_temperature_f %in% outlier1)) %>%
  filter(!(min_temperature_f %in% outlier2)) %>%
  filter(!(max_wind_speed_mph %in% outlier3)) %>%
  filter(!(mean_wind_speed_mph %in% outlier4)) %>%
  filter(!(max_gust_speed_mph %in% outlier5))

##################################################################################

## Writing the cleaned trip & weather csv.files ##
write.csv(trip3, file = "trip_clean.csv")
write.csv(weather4, file = "weather_clean.csv")


