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


## Reading the cleaned csv files into dataframes
trip_clean <- read.csv("trip_clean.csv")
station <- read.csv("station.csv")
weather_clean <- read.csv("weather_clean.csv")


##################################################################################

### Rush Hours Task ###

# Add rush hours to the dataset; find the hours of weekdays where the trip volume is highest

## Set-up ##
library(lubridate)
install.packages("tidyr")
library("tidyr")


## Data tranformation ##
trip4 <- trip_clean %>%
  # Creating new variable trip_day
  mutate(start_date = as.POSIXct(start_date, format="%m/%d/%Y %H:%M")) %>%
  mutate(trip_day = wday(start_date, label=TRUE, abbr=FALSE)) %>%
  
  # Subsetting the weekdays 
  filter(trip_day != "Saturday") %>%
  filter(trip_day != "Sunday") %>%
  
  # Extract time from start_date
  mutate(start_hour = hour(start_date))

# Shows the counts of weekdays
dplyr::count(trip4, trip4$trip_day)

trip5 <- trip_clean %>%
  # trip 5 includes weekdays and weekends
  mutate(start_date = as.POSIXct(start_date, format="%m/%d/%Y%H:%M")) %>%
  mutate(trip_day = wday(start_date, label=TRUE, abbr=FALSE))


## GGPlots showing highest trip volume ##

install.packages("ggpubr")
library("ggpubr")

allggplots <- ggarrange(ggplot(trip4, aes(start_hour)) + geom_density(),
                        ggplot(tripmonday, aes(start_hour)) + geom_density(),
                        ggplot(triptuesday, aes(start_hour)) + geom_density(),
                        ggplot(tripwed, aes(start_hour)) + geom_density(),
                        ggplot(tripthurs, aes(start_hour)) + geom_density(),
                        ggplot(tripfri, aes(start_hour)) + geom_density(),
                        labels = c("A", "B", "C", "D", "E", "F"),
                        ncol = 3, nrow = 2)
allggplots

# Overall rush hours for weekdays

densityX <- density(trip4$start_hour)$x
densityY <- density(trip4$start_hour)$y

rushhour <- densityX[which(diff(sign(diff(densityY)))==-2)]
rushhour

# Rush hours for Mondays

tripmonday <- filter(trip4, trip4$trip_day == "Monday")

densitymondayX <- density(tripmonday$start_hour)$x
densitymondayY <- density(tripmonday$start_hour)$y

rushhourmonday <- densitymondayX[which(diff(sign(diff(densitymondayY)))==-2)]
rushhourmonday

# Rush hours for Tuesdays

triptuesday <- filter(trip4, trip4$trip_day == "Tuesday")

densitytuesdayX <- density(triptuesday$start_hour)$x
densitytuesdayY <- density(triptuesday$start_hour)$y

rushhourtuesday <- densitytuesdayX[which(diff(sign(diff(densitytuesdayY)))==-2)]
rushhourtuesday

# Rush hours for Wednesday

tripwed <- filter(trip4, trip4$trip_day == "Wednesday")

densitywedX <- density(tripwed$start_hour)$x
densitywedY <- density(tripwed$start_hour)$y

rushhourwed <- densitywedX[which(diff(sign(diff(densitywedY)))==-2)]
rushhourwed

# Rush hours for Thursday

tripthurs <- filter(trip4, trip4$trip_day == "Thursday")

densitythursX <- density(tripthurs$start_hour)$x
densitythursY <- density(tripthurs$start_hour)$y

rushhourthurs <- densitythursX[which(diff(sign(diff(densitythursY)))==-2)]
rushhourthurs

# Rush hours for Friday

tripfri <- filter(trip4, trip4$trip_day == "Friday")

densityfriX <- density(tripfri$start_hour)$x
densityfriY <- density(tripfri$start_hour)$y

rushhourfri <- densityfriX[which(diff(sign(diff(densityfriY)))==-2)]
rushhourfri

# Finding the 10 most frequent start and end stations during rush hours

trip7 <- trip4 %>%
  filter(start_hour == 8 | start_hour == 17)

head(dplyr::count(trip7, trip7$start_station_name, sort = T), 10)
head(dplyr::count(trip7, trip7$end_station_name, sort = T), 10)

# Finding the 10 most frequent start and end stations on weekends

tripweekend <- trip5 %>%
  filter(trip_day == "Saturday" | trip_day == "Sunday")

head(dplyr::count(tripweekend, tripweekend$start_station_name, sort = T), 10)
head(dplyr::count(tripweekend, tripweekend$end_station_name, sort = T), 10)

# Using cbind to observe any overlap in station names on weekday rush hours and weekends

cbind(head(dplyr::count(tripweekend, tripweekend$start_station_name, sort = T), 10),
      head(dplyr::count(trip7, trip7$start_station_name, sort = T), 10))

cbind(head(dplyr::count(tripweekend, tripweekend$end_station_name, sort = T), 10),
      head(dplyr::count(trip7, trip7$end_station_name, sort = T), 10))

# Finding average utilization

trip_avg_utilize <- trip5 %>%
  group_by(month(start_date)) %>%
  summarise_at(vars(duration), list(totalduration = sum))

trip_avg_utilize$`month(start_date)` <- month.abb[trip_avg_utilize$`month(start_date)`]


trip_avg_utilize$AvgUtilization <- ifelse(trip_avg_utilize$`month(start_date)` == "Jan"|
                                            trip_avg_utilize$`month(start_date)` == "Mar"|
                                            trip_avg_utilize$`month(start_date)` == "May"|
                                            trip_avg_utilize$`month(start_date)` == "Jul"|
                                            trip_avg_utilize$`month(start_date)` == "Aug"|
                                            trip_avg_utilize$`month(start_date)` == "Oct"|
                                            trip_avg_utilize$`month(start_date)` == "Dec",
                                          trip_avg_utilize$totalduration/2678400, 
                                          # Months with 31 days
                                          
                                          ifelse(trip_avg_utilize$`month(start_date)` == "Apr"|
                                                   trip_avg_utilize$`month(start_date)` == "Jun"|
                                                   trip_avg_utilize$`month(start_date)` == "Sep"|
                                                   trip_avg_utilize$`month(start_date)` == "Nov",
                                                 trip_avg_utilize$totalduration/2592000, 
                                                 # Months with 30 days
                                                 
                                                 trip_avg_utilize$totalduration/2419200))
                                                # February


barplot(trip_avg_utilize$AvgUtilization, names.arg = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), main = "Average Utilization in Each Month", xlab = "Months", ylab = "Average Utilization")
