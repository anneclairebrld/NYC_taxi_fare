library(readr)
library(dplyr)
install.packages('geosphere')
library(geosphere)
library(ggplot2)
install.packages('lubridate')
library(lubridate)
setwd('C:/Users/kleok/OneDrive/Desktop/Master in DSBA/Semester 2/ESSEC/Big Data Analytics/NYC_taxi_fare') # set this as working directory

# read only first 100 lines as too much data 
file_in <-file('./train.csv', 'r')
chunk_size <- 1000
# Hello 


df <- read.csv(file = './data/train.csv', nrows=100000) 

#reformat date and time
df$pickup_date <- as.Date(df$pickup_datetime)
df$pickup_time <- format(as.POSIXct(df$pickup_datetime), '%H:%M:%S')

# remove redundant columns
df <- df[, !names(df) %in% c('key', 'pickup_datetime')]
head(df)

# check the values of longitude and latitude
print(min(df$pickup_longitude))
print(max(df$pickup_longitude)) 
print(min(df$dropoff_longitude))
print(max(df$dropoff_latitude))
print(subset(df, df$dropoff_latitude > 90))
print(subset(df, df$pickup_longitude < -180))

#remove cases where the longitude and lattitude are false
df <- subset(df, !df$dropoff_latitude > 90)
df <- subset(df, !df$pickup_latitude > 90)
df <- subset(df, !df$pickup_longitude < -180)
head(df)
# compute euclidean distance from latitude-longitude 
df$distance_euclidean <- round(distHaversine (df[c('pickup_longitude', 'pickup_latitude')], df[c('dropoff_longitude','dropoff_latitude')]) / 1000,1)

# only keep when euclidean distance is less than 100k 
ggplot(data  = df, aes(y = df$distance_euclidean)) +
  geom_boxplot() +
  scale_y_continuous('Euclidean distance') +
  ggtitle('Travel distance dispersion') +
  theme(axis.title.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank())

df <- subset(df, df$distance_euclidean < 100)


df$pickup_hour <- round(as.numeric(format(strptime(df$pickup_time, '%H:%M:%S'), "%H")) +
  as.numeric(format(strptime(df$pickup_time, '%H:%M:%S'), "%M"))/60 )

df$week = 0
df$day_of_week <- weekdays(as.POSIXct(df$pickup_date), abbreviate = F)
df$week <- cut.Date(df$pickup_date, breaks = "1 week", labels = FALSE)
df$pickup_hour[df$pickup_hour == 24] <- 0
tail(df)

occ <- df %>% count(df$day_of_week, df$week, df$pickup_hour)
head(occ)

# plottind pickuptime vs price 
ggplot(data = df, aes(x = df$distance_euclidean, group = df$distance_euclidean, y = df$fare_amount)) +
  geom_boxplot() +
  scale_x_continuous('Number of kms')+
  scale_y_continuous('Taxi fare') +
  ggtitle('Price of the Taxi vs distance')

# count by week or something 
ggplot(data = occ, aes(x = occ$`df$day_of_week`, group = occ$`df$day_of_week`, y=occ$n)) +
  geom_boxplot() +
  scale_x_discrete('Day of the week', limits = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday')) +
  scale_y_continuous('Number of taxi rides') +
  ggtitle('Dispersion of number of taxis per day for each week')
#amout of taxis taken 

sub_df <- subset(df, year(df$pickup_date) == 2009) 
occ <- sub_df %>% count(sub_df$day_of_week, sub_df$pickup_hour)
head(occ)


# plot number of taxis ordered for each
ggplot(data = occ, aes(x = occ$`sub_df$pickup_hour`, group = occ$`sub_df$day_of_week`, y=occ$n)) +
  geom_point(aes(colour = factor(occ$`sub_df$day_of_week`))) +
  geom_line(aes(colour = factor(occ$`sub_df$day_of_week`))) +
  scale_color_discrete('Weekday', label=c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday')) +
  scale_x_discrete('Hour of the day', limits = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23)) +
  scale_y_continuous('Number of taxi rides') +
  ggtitle('Number of rides by hour of the day across weekdays (year 2015)')


