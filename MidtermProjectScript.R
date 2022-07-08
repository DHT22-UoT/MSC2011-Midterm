#' MSC2011 Midterm Project
#' Muhammad Musa, Danni Ma

# Downloading and calling relevant packaages
install.packages("tidyverse")
install.packages("funModeling")
install.packages("Hmisc")

library(funModeling) 
library(tidyverse) 
library(Hmisc)

trip <- read.csv("trip.csv")
trip
##################################################################################
# Explanatory data analysis (EDA) for trip
trip_eda <- function(trip)
{
  glimpse(trip)
  print(status(trip))
  freq(trip) 
  print(profiling_num(trip))
  plot_num(trip)
  describe(trip)
}

glimpse(trip)
status(trip)
freq(trip)

plot_num(trip)
trip_prof=profiling_num(trip)

describe(trip)

##################################################################################