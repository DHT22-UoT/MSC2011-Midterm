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
library("tidyr")


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


trip5 <- trip3 %>%
  # trip 5 includes weekdays and weekends
  mutate(start_date = as.POSIXct(start_date, format="%m/%d/%Y%H:%M")) %>%
  mutate(trip_day = wday(start_date, label=TRUE, abbr=FALSE))
  

## GGPlots showing highest trip volume ##

# Overall rush hours for weekdays

ggplot(trip4, aes(start_hour)) + geom_density()

densityX <- density(trip4$start_hour)$x
densityY <- density(trip4$start_hour)$y

rushhour <- densityX[which(diff(sign(diff(densityY)))==-2)]
rushhour

# Rush hours for Mondays

tripmonday <- filter(trip4, trip4$trip_day == "Monday")

ggplot(tripmonday, aes(start_hour)) + geom_density()

densitymondayX <- density(tripmonday$start_hour)$x
densitymondayY <- density(tripmonday$start_hour)$y

rushhourmonday <- densitymondayX[which(diff(sign(diff(densitymondayY)))==-2)]
rushhourmonday

# Rush hours for Tuesdays

triptuesday <- filter(trip4, trip4$trip_day == "Tuesday")

ggplot(triptuesday, aes(start_hour)) + geom_density()

densitytuesdayX <- density(triptuesday$start_hour)$x
densitytuesdayY <- density(triptuesday$start_hour)$y

rushhourtuesday <- densitytuesdayX[which(diff(sign(diff(densitytuesdayY)))==-2)]
rushhourtuesday

# Rush hours for Wednesday

tripwed <- filter(trip4, trip4$trip_day == "Wednesday")

ggplot(tripwed, aes(start_hour)) + geom_density()

densitywedX <- density(tripwed$start_hour)$x
densitywedY <- density(tripwed$start_hour)$y

rushhourwed <- densitywedX[which(diff(sign(diff(densitywedY)))==-2)]
rushhourwed

# Rush hours for Thursday

tripthurs <- filter(trip4, trip4$trip_day == "Thursday")

ggplot(tripthurs, aes(start_hour)) + geom_density()

densitythursX <- density(tripthurs$start_hour)$x
densitythursY <- density(tripthurs$start_hour)$y

rushhourthurs <- densitythursX[which(diff(sign(diff(densitythursY)))==-2)]
rushhourthurs

# Rush hours for Friday

tripfri <- filter(trip4, trip4$trip_day == "Friday")

ggplot(tripfri, aes(start_hour)) + geom_density()

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

dplyr::count(tripweekend, tripweekend$trip_day)
head(dplyr::count(tripweekend, tripweekend$start_station_name, sort = T), 10)
head(dplyr::count(tripweekend, tripweekend$end_station_name, sort = T), 10)

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
                                          
                                          ifelse(trip_avg_utilize$`month(start_date)` == "Apr"|
                                                   trip_avg_utilize$`month(start_date)` == "Jun"|
                                                   trip_avg_utilize$`month(start_date)` == "Sep"|
                                                   trip_avg_utilize$`month(start_date)` == "Nov",
                                                 trip_avg_utilize$totalduration/2592000, 
                                                 
                                                 trip_avg_utilize$totalduration/2419200))


