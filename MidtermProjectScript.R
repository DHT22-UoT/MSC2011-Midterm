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