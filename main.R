library(readr)
library(dplyr)
library(rgdal)
library(geosphere)
library(ggplot2)
library(lubridate)
library(sp)
library(st)
library(sf)
library(raster)
library(tmap)
library(ggmap)

## remove bad rows from initial dataset 
remove_bad_rows <- function(df){
  # remove unfeasible rows for latitude and longitude
  df <- subset(df, !df$dropoff_latitude > 90)
  df <- subset(df, !df$pickup_latitude > 90)
  df <- subset(df, !df$pickup_longitude < -180)
  df <- subset(df, !df$dropoff_longitude < -180)
  df <- subset(df, !df$pickup_longitude > 180)
  df <- subset(df, !df$dropoff_longitude > 180)
  df <- subset(df, !df$pickup_latitude < -90)
  df <- subset(df, !df$dropoff_latitude < -90)
 
  
  # remove rows for passengers = 0 
  df <- subset(df, !df$passenger_count == 0)
  
  # remove rows when the price is negative (corresponds to refund)
  df <- subset(df, !df$fare_amount <= 0)
  
  return(df)
}

# separate date and time 
date_time_features <- function(df){
  df$pickup_date <- as.Date(df$pickup_datetime)
  df$pickup_time <- format(as.POSIXct(df$pickup_datetime), '%H:%M:%S') 
  
  return(df)
}

# compute distance between pickup and dropoff 
compute_distance <- function(df, type='euclidean'){
  if(type == 'euclidean') { # in kms
    df$distance_kms <- round(distHaversine (df[c('pickup_longitude', 'pickup_latitude')], df[c('dropoff_longitude','dropoff_latitude')]) / 1000,1)
    
    return(df)
  } else {# case when computing shortest distance # to implement
    
    return(df)
  }
}

# get day of week from date
get_day_of_week <- function(df){
  df$weekday <- weekdays(as.POSIXct(df$pickup_date), abbreviate = F)
  
  return(df)
}

# encode if worday or weekend
worday_weekend_feature <- function(df){
  df$workday <- 1 
  df$workday[df$weekday %in% c('Saturday', 'Sunday')] <- 0
  
  return(df)
}

# round time to nearest hour 
round_to_hour <- function(df){
  df$pickup_hour <- round(as.numeric(format(strptime(df$pickup_time, '%H:%M:%S'), "%H")) +
                            as.numeric(format(strptime(df$pickup_time, '%H:%M:%S'), "%M"))/60 )
  df$pickup_hour[df$pickup_hour == 24] <- 0
  
  return(df)
}

# log base(10) amount -> maybe should not be base 10
log_amount <- function(df){
  df$log_amount <- log10(df$fare_amount)
  return(df)
}

set_up_google_api <- function(df){
  my_key =  'AIzaSyDnZwhF1o5Yc5XcOfW1oCZSNU7Z5Dpg5pI'
  register_google(my_key, write = TRUE)
}

plot_heat_map <- function(df){
  # get map of manhattan 
  manhattan <- get_map("new york", zoom = 12, color = "bw")
  
  # concentrate on manhattan 
  df <- df %>% filter( between(pickup_latitude, 40.70, 40.83) & between(pickup_longitude, -74.025, -73.93) )
  
  ggmap(manhattan, darken = 0.5) +
    scale_fill_viridis(option = 'plasma') +
    geom_bin2d(data = df, aes(pickup_longitude, pickup_latitude), bins=60, alpha=0.6) +
    labs(x = "pickup longitude", y = "pickup latitude", fill = "concentration")
}

# set working dir to my file 
setwd('~/workspace/NYC_taxi_fare')

# read data, choose subset 
train_df <- read.csv(file = './data/train.csv', nrows=100000) 
head(train_df)


# do feature engineering
train_df <- remove_bad_rows(train_df)
train_df <- date_time_features(train_df)
train_df <- compute_distance(train_df)
train_df <- get_day_of_week(train_df)
train_df <- round_to_hour(train_df)
train_df <- get_day_of_week(train_df)
train_df <- worday_weekend_feature(train_df)
train_df <- log_amount(train_df)

# select final year (check data for now I am taking this)
train_df <- subset(train_df, train_df$pickup_date >= '2014-06-30') 

# remove redundant columns
train_df <- train_df[, !names(train_df) %in% c('key', 'pickup_datetime')]
head(train_df)

print(train_df[train_df$fare_amount == 0.01, 'distance_kms'])
print(min(train_df$fare_amount))
print(max(train_df$pickup_date))
print(nrow(train_df))


