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
library(tidyverse)

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

## classification
classify_fare_amount <- function(df) {
  df$fare_amount <- as.numeric(df$fare_amount)
  df <- df %>% mutate(fare_amount_class = case_when(
    between(fare_amount, 0, 3) ~ '0-5',
    between(fare_amount, 5, 10) ~ '5-10',
    between(fare_amount, 10, 15) ~ '10-15',
    between(fare_amount, 15, 20) ~ '15-20', 
    fare_amount > 20 ~ '20+'
  ))
  
  df$fare_amount_class <- as.factor(df$fare_amount_class)
  
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

cross_validation <- function(df, model, formula, model_type, nfolds=10) {
  if(model_type == 'regression'){
    test.rmse <- rep(0, nfolds)
    test.mse <- rep(0, nfolds)
  }
  df <- df[sample(nrow(df)), ]
  index <- rep(1:nfolds, length.out = nrow(df))
  
  for(i in 1:nfolds){
    insample  = which(index != i)
    outsample = which(index == i)
    
    X_test <- df[insample, ]
    X_train <- df[outsample, ]
    
    if(model == 'randomForest'){
      amount_model<- randomForest(formula, X_train, ntree = 100, na.action = na.omit)
    }else if (model == 'tree'){
      amount_model <- tree(formula, data = X_train)
    }else if (model == 'lm'){
      amount_model <- lm(formula, data = X_train)
    }
  
    predictions  <- predict(amount_model, X_test)
    
    if(model_type == 'classifier') {
      target <- X_test$fare_amount_class
      
      cm <- confusionMatrix(target, predictions)
      print((amount_model$confusion))
      print(cm$overall)
    }else {
      target <- X_test$fare_amount
      test.rmse[i] =  RMSE(target, predictions)
      test.mse[i] =  mean((target -  predictions)^2)
      cat(c('RMSE: ', test.rmse[i], ' MSE: ', test.mse[i], '\n'))
    }
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
train_df <- classify_fare_amount(train_df)

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

amount_formula2 = fare_amount_class ~ pickup_latitude + pickup_longitude + month_feature +  weekday +
  dropoff_longitude + dropoff_latitude + passenger_count + pickup_hour

cross_validation(train_df, 'randomForest', amount_formula2, 'classifier')
#cross_validation(train_df, 'randomForest', amount_formula, 'regression')
#cross_validation(train_df, 'lm', amount_formula, 'regression')
#cross_validation(train_df, 'tree', amount_formula2, 'regression')

# Get results from last cross validation and make a df 
pred_df <- data.frame(true = X_test$fare_amount_class, preds = predictions_rf)

