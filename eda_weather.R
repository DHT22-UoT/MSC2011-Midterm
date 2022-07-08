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

weather <- read.csv("weather.csv")

## First approach to data ##
glimpse(weather)
status(weather)

## Analyzing categorical variables ##
freq(weather)
freq(weather, path_out = ".")

# Analyzing numerical variables
plot_num(weather)
plot_num(weather, path_out = ".")

weather_prof <- profiling_num(weather)

# Analyzing categorical & numerical variables at the same time 
describe(weather)

