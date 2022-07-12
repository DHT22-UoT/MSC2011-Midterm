#' MSC2011 Midterm Project
#' Muhammad Musa, Danni Ma

### Exploratory Data Analysis: Trip data ###

## Set up ##
install.packages("tidyverse")
install.packages("funModeling")
install.packages("Hmisc")

library(funModeling)
library(tidyverse)
library(Hmisc)

trip <- read.csv("trip.csv", na.strings = "")

## First approach to data ##
glimpse(trip)
  # 326339 observations & 11 variables

status(trip)
  # zip_code has 50 zero values and 1493 missing values

## Analyzing categorical variables ##
freq(trip)
  #' The "San Francisco Caltrain (Townsend at 4th)" station had the highest frequency 
  #' as starting station (25144) & end station (33213)

freq(trip, path_out = ".")

# Analyzing numerical variables
plot_num(trip)
  # 1 large peak: duration
  # Negatively skewed: start_station_id, end_station_id
  # Require conversion into factor: start_station_name, end_station_name

plot_num(trip, path_out = ".")

trip_prof <- profiling_num(trip)
  # Skewness: duration (539.65)
  # High std: duration (30816.16)
  # High variation_coef: duration(27.22)

# Analyzing categorical & numerical variables at the same time 
describe(trip)


