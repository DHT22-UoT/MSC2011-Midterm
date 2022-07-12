#' MSC2011 Midterm Project
#' Muhammad Musa, Danni Ma

### Exploratory Data Analysis: Weather data ###

## Set up ##
install.packages("tidyverse")
install.packages("funModeling")
install.packages("Hmisc")

library(funModeling)
library(tidyverse)
library(Hmisc)

weather <- read.csv("weather.csv", na.strings = "")

## First approach to data ##
glimpse(weather)
  # 1825 observations & 15 variables

status(weather)
  # max_gust_speed_mph has 451 missing values
  # precipitation_inches has 1543 zero values 
  # cloud_cover, events, zip_code, and city are categorical variables (require conversion into factors)

## Analyzing categorical variables ##
freq(weather)
  # precipitation_inches: most observations (1543) have 0 inch precipitation, 73 has "T", numerical data treated as categorical
  # events: most observations (1473) have NAs; level "rain" can be combined with level "Rain"
  # city: all 5 cities have the same frequency

freq(weather, path_out = ".")

# Analyzing numerical variables
plot_num(weather)
  # No significant sign of outlier: max_temperature_f, mean_temperature_f, min_temperature_f, mean_wind_speed_mph
  # 1 large peak, relatively symmetrical: max_visibility_miles, mean_visibility_miles
  # Potential outlier: min_visibility_miles, max_wind_Speed_mph, max_gust_speed_mph
  # Require conversion into factor: cloud_cover, zip_code

plot_num(weather, path_out = ".")

weather_prof <- profiling_num(weather)
  # Skewness: max_wind_Speed_mph (7.47), max_gust_speed_mph (4.93), max_visibility_miles (2.88)
  # High std: max_gust_speed_mph (9.09), max_temperature_f (8.26), max_wind_Speed_mph (7.32)
  # High variation_coef: mean_wind_speed_mph (0.50), max_wind_Speed_mph (0.45), max_gust_speed_mph (0.40)

# Analyzing categorical & numerical variables at the same time 
describe(weather)

