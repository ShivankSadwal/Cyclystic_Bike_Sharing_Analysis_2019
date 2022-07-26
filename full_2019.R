# Divvy Trips 2019 Full Year Analysis

#Installing necessary packages
install.packages("tidyverse")
install.packages("lubridate")
install.packages("ggplot2")

#loading 
library(tidyverse)  #helps wrangle data
library(lubridate)  #helps wrangle date attributes
library(ggplot2)  #helps in visualization

#setting work directory for easy access to csv files
setwd("C:/Users/ahlco/Desktop/CASE STUDY 1/Divvy_trips_2019")


#Getting the data
q1_2019 <- read.csv("Divvy_Trips_2019_Q1.csv")
q2_2019 <- read.csv("Divvy_Trips_2019_Q2.csv")
q3_2019 <- read.csv("Divvy_Trips_2019_Q3.csv")
q4_2019 <- read.csv("Divvy_Trips_2019_Q4.csv")


#inspecting data
colnames(q1_2019)
colnames(q2_2019)
colnames(q3_2019)
colnames(q4_2019)


#Renaming all columns to latest naming scheme
q1_2019 <- rename(q1_2019,ride_id = trip_id
                  ,rideable_type = bikeid 
                  ,started_at = start_time  
                  ,ended_at = end_time  
                  ,start_station_name = from_station_name 
                  ,start_station_id = from_station_id 
                  ,end_station_name = to_station_name 
                  ,end_station_id = to_station_id 
                  ,member_casual = usertype)


q2_2019 <- rename(q2_2019,ride_id = X01...Rental.Details.Rental.ID
                  ,rideable_type = X01...Rental.Details.Bike.ID
                  ,started_at = X01...Rental.Details.Local.Start.Time  
                  ,ended_at = X01...Rental.Details.Local.End.Time 
                  ,start_station_name = X03...Rental.Start.Station.Name 
                  ,start_station_id = X03...Rental.Start.Station.ID
                  ,end_station_name = X02...Rental.End.Station.Name 
                  ,end_station_id = X02...Rental.End.Station.ID
                  ,member_casual = User.Type)



q3_2019 <- rename(q3_2019
                  ,ride_id = trip_id
                  ,rideable_type = bikeid 
                  ,started_at = start_time  
                  ,ended_at = end_time  
                  ,start_station_name = from_station_name 
                  ,start_station_id = from_station_id 
                  ,end_station_name = to_station_name 
                  ,end_station_id = to_station_id 
                  ,member_casual = usertype)


q4_2019 <- rename(q4_2019
                  ,ride_id = trip_id
                  ,rideable_type = bikeid 
                  ,started_at = start_time  
                  ,ended_at = end_time  
                  ,start_station_name = from_station_name 
                  ,start_station_id = from_station_id 
                  ,end_station_name = to_station_name 
                  ,end_station_id = to_station_id 
                  ,member_casual = usertype)


# Inspect the dataframes and look for incongruencies
str(q1_2019)
str(q4_2019)
str(q3_2019)
str(q2_2019)


# Convert ride_id and rideable_type to character so that they can stack correctly
q4_2019 <-  mutate(q4_2019, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 
q3_2019 <-  mutate(q3_2019, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 
q2_2019 <-  mutate(q2_2019, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 
q1_2019 <-  mutate(q1_2019, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 


# Stack individual quarter's data frames into one big data frame

all_trips <- bind_rows(q1_2019,q2_2019, q3_2019, q4_2019)



colnames(all_trips)


#removing all columns that are not a part of the new structure.

all_trips <- all_trips %>%  
  select(-c("birthyear","gender","X01...Rental.Details.Duration.In.Seconds.Uncapped",
            "X05...Member.Details.Member.Birthday.Year", "Member.Gender", "tripduration"))

colnames(all_trips)

head(all_trips)


# Inspect the new table that has been created
colnames(all_trips)  #List of column names
nrow(all_trips)  #How many rows are in data frame?
dim(all_trips)  #Dimensions of the data frame?
head(all_trips)  #See the first 6 rows of data frame.  Also tail(all_trips)
str(all_trips)  #See list of columns and data types (numeric, character, etc)
summary(all_trips)  #Statistical summary of data. Mainly for numerics



#CLEANING THE DATA

table(all_trips$member_casual)


#According to current schema Customer needs to be replaced with casual and Subscriber with member

all_trips <- all_trips %>% 
  mutate(member_casual = recode(member_casual,"Customer" = "casual","Subscriber" = "member"))

#Check if values have changed successfully
table(all_trips$member_casual)



#ADDING DATE, MONTH, DAY and YEAR as columns to make aggregation more easier.

all_trips$date <- as.Date(all_trips$started_at)  #default format is yyyy-mm-dd

#for custom formats we use format function 

all_trips$month <- format(as.Date(all_trips$date),"%m")

all_trips$day <- format(as.Date(all_trips$date),"%d")

all_trips$year <- format(as.Date(all_trips$date),"%Y")

all_trips$day_of_week <- format(as.Date(all_trips$date),"%A")


#Creating ride_length column to make calculations easier

all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)

#checking if new columns were added
str(all_trips)

# Convert "ride_length" from Factor to numeric so we can run calculations on the data
is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)

# Remove "bad" data
# The dataframe includes a few hundred entries when bikes were taken out of docks and checked for quality by Divvy or ride_length was negative
# We will create a new version of the dataframe (v2) since data is being removed

all_trips_v2 <- all_trips[!(all_trips$start_station_id == "HQ QR"|all_trips$ride_length<0),]


#SUMMARY OF NEW DATA FRAME
summary(all_trips_v2)
glimpse(all_trips_v2)

#SUMMARY OF SPECIFIC DATA ATTRIBUTE
summary(all_trips_v2$ride_length)

#COMPARING CASUAL USERS WITH MEMBERS
#MEAN
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)

#MEDIAN
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)

#MAX
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)

#MIN
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)



# See the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)


#ORDERING THE LEVELS ACCORDING TO WEEKDAY
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week , levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))


# See the average ride time by each day for members vs casual users after ordering weekday
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)



# analyze ridership data by type and weekday and make a dataframe weekly 
weekly <- all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual,weekday) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual,weekday)

#analyse ridership data by member type and month and make dataframe monthly
monthly <- all_trips_v2 %>% 
  group_by(member_casual,month) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual,month)
 

#analyze ridership data by member type and start station name and making a dataframe named geo for later getting geolocation data from

geo <- all_trips_v2 %>% 
   group_by(member_casual,start_station_name) %>% 
   summarise(number_of_rides = n() , average_duration = mean(ride_length)) %>% 
   arrange(member_casual,start_station_name)


#using day_of_week instead of making weekday
all_trips_v2 %>% 
  group_by(member_casual,day_of_week) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual,day_of_week)



#Grouping by member_casual and month and finding the number of rides and average ride duration
all_trips_v2 %>% 
  group_by(member_casual,month) %>% 
  summarise(number_of_rides = n(),average_duration = mean(ride_length)) %>% 
  arrange(member_casual,month)

#  Visualize the number of rides by rider type
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual,weekday) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual,weekday) %>% 
  ggplot(aes(x=weekday,y=number_of_rides,fill = member_casual))+
  geom_col(position = "dodge")


# Let's create a visualization for average duration
all_trips_v2 %>% 
  #mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, day_of_week)  %>% 
  ggplot(aes(x = day_of_week, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")


#SAME VISUALIZATIONS FOR MONTH INSTEAD OF WEEKDAY


#  Visualize the number of rides by rider type
all_trips_v2 %>% 
  group_by(member_casual,month) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual,month) %>% 
  ggplot(aes(x=month,y=number_of_rides,fill = member_casual))+
  geom_col(position = "dodge")


# Let's create a visualization for average duration
all_trips_v2 %>% 
  group_by(member_casual, month) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, month)  %>% 
  ggplot(aes(x = month, y = average_duration,fill = member_casual )) +
  geom_col(position = "dodge")


# EXPORT SUMMARY FILEs FOR FURTHER ANALYSIS

counts <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)
write.csv(counts, file = 'C:/Users/ahlco/Desktop/CASE STUDY 1/Divvy_trips_2019/csv_divvy_trips_2019/avg_ride_length.csv')


write.csv(all_trips_v2, file = 'C:/Users/ahlco/Desktop/CASE STUDY 1/Divvy_trips_2019/csv_divvy_trips_2019/all_trips_v2.csv')

#exporting geo data for the year 2019
write.csv(geo, file = 'C:/Users/ahlco/Desktop/CASE STUDY 1/cyclistic_geographic_data_2019.csv')

#Exporting weekly data
write.csv(weekly, file = 'C:/Users/ahlco/Desktop/CASE STUDY 1/cyclistic_weekly_data_2019.csv')

#Exporting monthly data
write.csv(monthly, file = 'C:/Users/ahlco/Desktop/CASE STUDY 1/cyclistic_monthly_data_2019.csv')