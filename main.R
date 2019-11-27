library(readr)
library(dplyr)
library(rgdal)
library(geosphere)
library(ggplot2)
library(lubridate)
library(ggmap)
library(tree)
library(caret)
library(randomForest)
library(pROC)
library(viridis)

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

get_only_manhattan_data <- function(df){
  df <- df %>% filter( between(dropoff_latitude, 40.70, 40.83) & between(dropoff_longitude, -74.025, -73.93) )
  
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
  df$weekday <- as.factor(df$weekday)
  
  return(df)
}

# encode if worday or weekend
worday_weekend_feature <- function(df){
  df$workday <- 1 
  df$workday[df$weekday %in% c('Saturday', 'Sunday')] <- 0
  df$workday <- as.factor(df$workday)
  
  return(df)
}

# get month and make it factor i.e. a month is a class
get_month_feature <- function(df){
  df$month_feature <- as.factor(month(df$pickup_date))
  return(df)
}

# round time to nearest hour 
round_to_hour <- function(df){
  df$pickup_hour <- round(as.numeric(format(strptime(df$pickup_time, '%H:%M:%S'), "%H")) +
                            as.numeric(format(strptime(df$pickup_time, '%H:%M:%S'), "%M"))/60 )
  df$pickup_hour[df$pickup_hour == 24] <- 0
  
  return(df)
}

# log base(10) amount -> maybe should not be base 10 ?
log_amount <- function(df){
  df$log_amount <- log10(df$fare_amount)
  return(df)
}

set_up_google_api <- function(){
  my_key =  'AIzaSyDnZwhF1o5Yc5XcOfW1oCZSNU7Z5Dpg5pI'
  register_google(my_key, write = TRUE)
}

# heat map plotting 
plot_heat_map <- function(df){
  # get map of manhattan 
  manhattan <- get_map("manhattan", zoom = 11, color = "bw")
  
  # concentrate on manhattan 
  get_only_manhattan_data(df)
  
  ggmap(manhattan, darken = 0.5) +
    scale_fill_viridis(option = 'plasma') +
    geom_bin2d(data = df, aes(dropoff_longitude, dropoff_latitude), bins=60, alpha=0.6) +
    labs(x = "pickup longitude", y = "pickup latitude", fill = "concentration")
}

# function to plots graphs  
plot_graphs <- function(df, graph = 'none'){
  if(graph == 'heatmap'){
    # plot heat maps -> only do if doing graphs
    set_up_google_api() # this should really be run only once
    plot_heat_map(df)
  }else if (graph == 'fare_amount_hist'){
    ggplot(data = df, aes(fare_amount)) + 
      geom_histogram(binwidth = 1)  +
      scale_x_continuous('Fare amount in dollars', limits = c(0, 60)) + 
      scale_y_continuous('Number of rides') + 
      ggtitle('Histogram of taxi rides fare amount')
  }
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
train_df <- worday_weekend_feature(train_df)
train_df <- get_month_feature(train_df)
#train_df <- log_amount(train_df)
train_df <- get_only_manhattan_data(train_df)

# select final year (check data for now I am taking this)
train_df <- subset(train_df, train_df$pickup_date >= '2014-06-30') 

# remove redundant columns
train_df <- train_df[, !names(train_df) %in% c('key', 'pickup_datetime')]
head(train_df)

# specify which graph you want to plot if any
plot_graphs(train_df, graph = 'fare_amount_hist')

## CROSS VALIDATION ON AMOUNT MODEL 
amount_formula = fare_amount ~ pickup_latitude + pickup_longitude + month_feature +  weekday +
      dropoff_longitude + dropoff_latitude + passenger_count + pickup_hour


# shuffle the data 
train_df <- train_df[sample(nrow(train_df)), ]
nfolds <- 10 
index <- rep(1:nfolds, length.out = nrow(train_df))
test.rmse <- rep(0, nfolds)
test.mse <- rep(0, nfolds)

for(i in 1:nfolds){
  insample  = which(index != i)
  outsample = which(index == i)
 
  X_test <- train_df[insample, ]
  X_train <- train_df[outsample, ]
  
  #amount_model <- tree(amount_formula, data = X_train)
  #amount_model <- lm(amount_formula, data = X_train)
  amount_model <- randomForest(amount_formula, X_train, ntree = 100, na.action = na.omit)
  predictions_rf  <- predict(amount_model, X_test)
  
  amount_model <- tree(amount_formula, data = X_train)
  predictions_tree  <- predict(amount_model, X_test)
  
  #test.rmse[i] =  RMSE(X_test$fare_amount, predictions) # mean squared test error
  #test.mse[i] =  mean((X_test$fare_amount -  predictions)^2)
  #train.rmse[i] = RMSE(X_train$fare_amount, predictions) # root meat squared 
  print((amount_model))
} 

## Amount model plots 
plot(amount_model)
# plot(train.rmse) 
#plot(test.rmse) 
#plot(test.mse) 



roc_rf <- roc(X_test$fare_amount, predictions_rf)
roc_tree <- roc(X_test$fare_amount, predictions_tree)

plot(roc_rf) + 
  lines(roc_tree, col='red')
# Get results from last cross validation and make a df 
pred_df <- data.frame(true = X_test$fare_amount, preds = predictions_rf)
