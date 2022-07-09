#' MSC2011 Midterm Project
#' Muhammad Musa, Danni Ma

# Downloading and calling relevant packaages
install.packages("tidyverse")
install.packages("funModeling")
install.packages("Hmisc")

library(funModeling) 
library(tidyverse) 
library(Hmisc)
library("dplyr")

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

# Filter out trips that are less than 2 mins. These are most likely cancelled trips

sum(trip$duration < 120)

trip1 <- trip %>%
  filter(duration >= 120)

# Identify the outliers - Duration

summary(trip1)

summary(trip1$duration)
hist(trip1$duration)
boxplot(trip1$duration)

trip1q <- quantile(trip1$duration)
trip1iqr <- IQR(trip1$duration) 

upperlimit <- trip1q[4] * 1.5
lowerlimit <- trip1q[2] * 1.5
lowerlimit <- trip1q[2] - 172.5

trip2 <- trip1 %>%
  filter(duration < upperlimit) %>%
  filter(duration > lowerlimit)
summary(trip2$duration)

##################################################################################
library(lubridate)

# adding rush hours to the dataset - highest volume of hours during weekdays
# I tried to first make a column that would say the day of each date - like monday, tues, wed, ...
# From this i was going to filter the weekdays only and find rush hours but for some reason only half the dataset shows the weekdays and the buttom half just say NA
# let me know if this makes sense and if you know what I did wrong.

tripst <- as.Date(trip2$start_date)

tripst1 <- as.POSIXct(tripst, format = "%d/%m/%y")
tripstartday <- wday(tripst1, label=TRUE, abbr=FALSE)

trip3 <- trip2 %>%
  mutate(trip.day = wday(tripst1, label=TRUE, abbr=FALSE))

