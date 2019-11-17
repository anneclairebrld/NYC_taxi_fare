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

# remove unfeasible rows for latitude and longitude
remove_bad_rows <- function(df){
  df <- subset(df, !df$dropoff_latitude > 90)
  df <- subset(df, !df$pickup_latitude > 90)
  df <- subset(df, !df$pickup_longitude < -180)
  df <- subset(df, !df$pickup_latitude < -90)
  df <- subset(df, !df$dropoff_longitude < -180)
  
  return(df)
}

# separate date and time 
date_time_features <- function(df){
  df$pickup_date <- as.Date(df$pickup_datetime)
  df$pickup_time <- format(as.POSIXct(df$pickup_datetime), '%H:%M:%S') 
  
  return(df)
}

compute_distance <- function(df, type='euclidean'){
  if(type == 'euclidean') { # in kms
    df$distance_kms <- round(distHaversine (df[c('pickup_longitude', 'pickup_latitude')], df[c('dropoff_longitude','dropoff_latitude')]) / 1000,1)
    
    return(df)
  } else {# case when computing shortest distance # to implement
    
    return(df)
  }
}

get_day_of_week <- function(df){
  df$day_of_week <- weekdays(as.POSIXct(df$pickup_date), abbreviate = F)
  
  return(df)
}

round_to_hour <function(df){
  df$pickup_hour <- round(as.numeric(format(strptime(df$pickup_time, '%H:%M:%S'), "%H")) +
                            as.numeric(format(strptime(df$pickup_time, '%H:%M:%S'), "%M"))/60 )
  df$pickup_hour[df$pickup_hour == 24] <- 0
  
  return(df)
}

# set working dir to my file 
setwd('~/workspace/NYC_taxi_fare')

# read data, choose subset 
train_df <- read.csv(file = './data/train.csv', nrows=50000) 
head(train_df)

train_df <- remove_bad_rows(train_df)
train_df <- date_time_features(train_df)
train_df <- compute_distance(train_df)

# remove redundant columns
train_df <- train_df[, !names(train_df) %in% c('key', 'pickup_datetime')]
head(train_df)

