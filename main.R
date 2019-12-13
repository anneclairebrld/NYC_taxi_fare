library(dplyr)
library(readr)
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
library(ggthemes)
library(gmapsdistance)
library(ggmap)
library(leaflet)
library(stringr)
library(ggExtra)
library(xgboost)
library(tidyverse)
library(httr)
library(broom.mixed)
library(tigris)
library(rgdal)

Sys.setenv(TZ='UTC')

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


# remove rows when fare_per_km is larger than 100
remove_large_fare_per_km <- function(df, limit) {
df <- subset(df, !df$fare_per_km >= limit)

return(df)
}

remove_zero_distance <- function(df){
  # remove rows with distance  equal to 0
  df <- subset(df, !df$distance_kms == 0)
  return(df)
}


week_within_month <- function(df) {
  df$week_within_month <- (((as.integer(df$day_feature) - 1)%/%7)+1)
  
  return(df)
}

airport_start_or_dest <- function(df){
  #NYC_bounding_box = c(-74.26, -73.71 ,  40.43 , 40.95)
  #JFK_bounding_box = c(-73.86, -73.75 ,  40.61 , 40.66)
  #LGA_bounding_box = c(-73.91, -73.82 ,  40.75 , 40.79)
  #EWR_bounding_box = c(-74.19, -74.15 , 40.67 , 40.70)
  
  df$NYC_dropoff <- ifelse(between(df$dropoff_longitude, -74.26, -73.71) & between(df$dropoff_latitude, 40.43, 40.95),1,0)
  df$JKF_dropoff <- ifelse(between(df$dropoff_longitude, -73.86, -73.75) & between(df$dropoff_latitude, 40.61, 40.66),1,0)
  df$LGA_dropoff <- ifelse(between(df$dropoff_longitude, -73.91, -73.82) & between(df$dropoff_latitude, 40.75, 40.79),1,0)
  df$EWR_dropoff <- ifelse(between(df$dropoff_longitude, -74.19, -73.15) & between(df$dropoff_latitude, 40.67, 40.70),1,0)
  
  df$NYC_pickup <- ifelse(between(df$pickup_longitude, -74.26, -73.71) & between(df$pickup_latitude, 40.43, 40.95),1,0)
  df$JKF_pickup <- ifelse(between(df$pickup_longitude, -73.86, -73.75) & between(df$pickup_latitude, 40.61, 40.66),1,0)
  df$LGA_pickup <- ifelse(between(df$pickup_longitude, -73.91, -73.82) & between(df$pickup_latitude, 40.75, 40.79),1,0)
  df$EWR_pickup <- ifelse(between(df$pickup_longitude, -74.19, -73.15) & between(df$pickup_latitude, 40.67, 40.70),1,0)
  
  df$trip_to_airport <- ifelse((df$LGA_dropoff == 1) | (df$LGA_pickup == 1) | (df$NYC_dropoff == 1) | (df$NYC_pickup == 1) |
                                 (df$JFK_dropoff == 1) | (df$JFK_pickup == 1) | (df$EWT_dropoff == 1) | (df$EWT_pickup == 1) , 1, 0)
  return(df)
}


add_NY_holidays <- function(df) {
  df$holiday = 0
  df[df$month_feature==1 & df$day_feature == 1 & df$year_feature == 2014,]$holiday = 1       # New Year
  df[df$month_feature==1 & df$day_feature == 20  & df$year_feature == 2014,]$holiday = 1       # Lincoln's day
  df[df$month_feature==2 & df$day_feature == 12 & df$year_feature == 2014,]$holiday = 1       # Washington's day
  df[df$month_feature==2 & df$day_feature == 17 & df$year_feature == 2014,]$holiday = 1       #  Memorial day
  df[df$month_feature==5 & df$day_feature == 26 & df$year_feature == 2014,]$holiday = 1       # Independence day
  df[df$month_feature==7 & df$day_feature == 4 & df$year_feature == 2014,]$holiday = 1       # Labor Day
  df[df$month_feature==9 & df$day_feature == 1 & df$year_feature == 2014,]$holiday = 1       # Columbus day
  df[df$month_feature==11 & df$day_feature == 11 & df$year_feature == 2014,]$holiday = 1       # Veteran's day
  df[df$month_feature==11 & df$day_feature == 27 & df$year_feature == 2014,]$holiday = 1       # Thanksgiving
  df[df$month_feature==12 & df$day_feature == 25 & df$year_feature == 2014,]$holiday = 1       # Christmas
  df[df$month_feature==1 & df$day_feature == 1 & df$year_feature == 2015,]$holiday = 1       # New Year
  df[df$month_feature==1 & df$day_feature == 19  & df$year_feature == 2015,]$holiday = 1       # Lincoln's day
  df[df$month_feature==2 & df$day_feature == 12 & df$year_feature == 2015,]$holiday = 1       # Washington's day
  df[df$month_feature==2 & df$day_feature == 16 & df$year_feature == 2015,]$holiday = 1       #  Memorial day
  df[df$month_feature==5 & df$day_feature == 25 & df$year_feature == 2015,]$holiday = 1       #  Memorial day
  
  return(df)
}

countdown_holidays <- function(df, holiday_date) {  # input mia imerominia kai tha vriskei +- 3
  hol_day = day(as.Date(holiday_date))
  hol_month = month(as.Date(holiday_date))
  for(i in (hol_day - 3):(hol_day - 1)){
    cat(sprintf("\"%f\" \"%f\"\n", i, hol_month))
    if (i== 0) {
      if(hol_month > 1) {
        df[df$month_feature== (hol_month-1)  &  df$day_feature == 31,]$countdown = 1
      }
      # if month == 1, we should go to the previous year, one year data in our case
    }
    else if (i== -1) {
      if(hol_month > 1) {
        df[df$month_feature== (hol_month-1)  &  df$day_feature == 30,]$countdown = 1
      }
    }
    else if (i== -2) {
      if(hol_month > 1) {
        df[df$month_feature== (hol_month-1)  &  df$day_feature == 29,]$countdown = 1
      }
    }
    else {
      if(hol_month > 1) {
        df[df$month_feature== hol_month  &  df$day_feature == i,]$countdown = 1
      }
    }
  }
  for(i in (hol_day+1):(hol_day+3)){
    cat(sprintf("\"%f\" \"%f\"\n", i, hol_month))
    df[df$month_feature== hol_month  &  df$day_feature == i,]$countdown = 1
  }  
  
  return(df)
}

# separate date and time 
date_time_features <- function(df){
  df$pickup_date <- as.Date(df$pickup_datetime)
  df$pickup_time <- format(as.POSIXct(df$pickup_datetime), '%H:%M:%S') 
  
  return(df)
}

# routing using google API

set_up_google_api <- function(){
  my_key =  'AIzaSyDnZwhF1o5Yc5XcOfW1oCZSNU7Z5Dpg5pI'
  register_google(my_key, write = TRUE)
}

ggmap::register_google(key = my_key)

###

routing_features <- function(df){
  df$pickup = str_replace_all(paste(as.character(df$pickup_latitude),
                                    "+", 
                                    as.character(df$pickup_longitude)),
                              " ",
                              "")
  
  df$dropoff = str_replace_all(paste(as.character(df$dropoff_latitude),
                                     "+", 
                                     as.character(df$dropoff_longitude)),
                               " ",
                               "")
  
  results = gmapsdistance(origin = df$pickup, 
                          destination = df$dropoff, 
                          combinations = "pairwise", 
                          mode = "driving", 
                          # dep_date = data$pickup_date,
                          # dep_time = data$pickup_time,
                          key = my_key)
  
  df$distance = results$Distance$Distance
  df$distance = df$distance/1000 # in km
  
  df$time = results$Time$Time
  df$time = data$time/60 # in minutes
  
  return(df)
}

get_only_manhattan_data <- function(df){
  df <- df %>% filter( between(dropoff_latitude, 40.70, 40.83) & between(dropoff_longitude, -74.025, -73.93) )
  df <- df %>% filter( between(pickup_latitude, 40.70, 40.83) & between(pickup_longitude, -74.025, -73.93) )
  
  return(df)
}

# compute distance between pickup and dropoff 
compute_distance <- function(df, type='euclidean'){
  if(type == 'euclidean') { # in kms
    df$distance_kms <- round(distHaversine (df[c('pickup_longitude', 'pickup_latitude')], df[c('dropoff_longitude','dropoff_latitude')]) / 1000,1)
    
    return(df)
  } else if (type == 'harvesine'){# case when computing shortest distance # to implement
    
      
    return(df)
  }
}

# compute fare per km
compute_fare_per_km <- function(df) {
  df$fare_per_km <- df$fare_amount/df$distance_kms
  return(df)
}


# get day of week from date
get_day_of_week <- function(df){
  df$weekday <- weekdays(as.POSIXct(df$pickup_date), abbreviate = F)
  df$weekday <- as.factor(df$weekday)
  
  return(df)
}

# get neighborhoods
get_neighborhood <- function(df){
  nyc_neighborhoods <- readOGR("C:/Users/kleok/OneDrive/Desktop/Master in DSBA/Semester 2/ESSEC/Big Data Analytics/NYC_taxi_fare/nyc-neighborhoods.geojson")
  summary(nyc_neighborhoods)
  nyc_neighborhoods_df <- tidy(nyc_neighborhoods)

  points <- data.frame(lat=df$pickup_latitude, lng=df$pickup_longitude)

  points_spdf <- points
  coordinates(points_spdf) <- ~lng + lat
  proj4string(points_spdf) <- proj4string(nyc_neighborhoods)

  matches <- over(points_spdf, nyc_neighborhoods)
  
  df$pickup_neighborhoods <- matches$name
  
  points <- data.frame(lat=df$dropoff_latitude, lng=df$dropoff_longitude)
  
  points_spdf <- points
  coordinates(points_spdf) <- ~lng + lat
  proj4string(points_spdf) <- proj4string(nyc_neighborhoods)
  matches <- over(points_spdf, nyc_neighborhoods)
  df$dropoff_neighborhoods <- matches$name
  
  return(df)
}


# encode if worday or weekend
worday_weekend_feature <- function(df){
  df$workday <- 1 
  df$workday[df$weekday %in% c('Saturday', 'Sunday')] <- 0
  #df$workday <- as.factor(df$workday)
  
  return(df)
}


# get month, day and make it factor i.e. a month is a class
get_month_day_feature <- function(df){
  #df$month_feature <- as.factor(month(df$pickup_date))
  #df$day_feature  <- as.factor(day(df$pickup_date))
  df$month_feature <- month(df$pickup_date)
  df$day_feature  <- day(df$pickup_date)
  df$year_feature <- year(df$pickup_date)
  return(df)
}


# get seasons from date
get_season_feature <- function(df){
  df$season_feature[as.factor(month(df$pickup_date))==12]<-'winter'
  df$season_feature[as.factor(month(df$pickup_date))==1]<-'winter'
  df$season_feature[as.factor(month(df$pickup_date))==2]<-'winter'
  df$season_feature[as.factor(month(df$pickup_date))==3]<-'spring'
  df$season_feature[as.factor(month(df$pickup_date))==4]<-'spring'
  df$season_feature[as.factor(month(df$pickup_date))==5]<-'spring'
  df$season_feature[as.factor(month(df$pickup_date))==6]<-'summer'
  df$season_feature[as.factor(month(df$pickup_date))==7]<-'summer'
  df$season_feature[as.factor(month(df$pickup_date))==8]<-'summer'
  df$season_feature[as.factor(month(df$pickup_date))==9]<-'fall'
  df$season_feature[as.factor(month(df$pickup_date))==10]<-'fall'
  df$season_feature[as.factor(month(df$pickup_date))==11]<-'fall'
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
    between(fare_amount, 0, 5) ~ '0-5',
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

#encode & drop weekday
encode_weekday <- function(df){
  weekday1 <- data.frame(df$X, df$weekday)
  weekday2 <- dummyVars("~ .", data = weekday1)
  weekday3 <- data.frame(predict(weekday2, newdata = weekday1))
  names(weekday3)[names(weekday3) == "df.X"] <- "X"
  names(weekday3)[names(weekday3) == "df.weekday.Lundi"] <- "Monday"
  names(weekday3)[names(weekday3) == "df.weekday.Mardi"] <- "Tuesday"
  names(weekday3)[names(weekday3) == "df.weekday.Mercredi"] <- "Wednesday"
  names(weekday3)[names(weekday3) == "df.weekday.Jeudi"] <- "Thursday"
  names(weekday3)[names(weekday3) == "df.weekday.Vendredi"] <- "Friday"
  names(weekday3)[names(weekday3) == "df.weekday.Samedi"] <- "Saturday"
  names(weekday3)[names(weekday3) == "df.weekday.Dimanche"] <- "Sunday"
  df <-merge(df,weekday3, by.x = 'X',by.y='X')
  rm(weekday1)
  rm(weekday2)
  rm(weekday3)
  df$weekday <- NULL
  return(df)
}


#encode & drop season
encode_season <- function(df){
  season1 <- data.frame(df$X, df$season_feature)
  season2 <- dummyVars("~ .", data = season1)
  season3 <- data.frame(predict(season2, newdata = season1))
  names(season3)[names(season3) == "df.X"] <- "X"
  names(season3)[names(season3) == "df.season_feature.summer"] <- "summer"
  names(season3)[names(season3) == "df.season_feature.winter"] <- "winter"
  names(season3)[names(season3) == "df.season_feature.spring"] <- "spring"
  names(season3)[names(season3) == "df.season_feature.fall"] <- "fall"
  df <-merge(df,season3, by.x = 'X',by.y='X')
  rm(season1)
  rm(season2)
  rm(season3)
  df$season_feature <- NULL
  return(df)
}

#encode & pickupneigh
encode_pickupneigh <- function(df){
  pickupneigh1 <- data.frame(df$X, df$pickup_neighborhoods)
  pickupneigh2 <- dummyVars("~ .", data = pickupneigh1)
  pickupneigh3 <- data.frame(predict(pickupneigh2, newdata = pickupneigh1))
  names(pickupneigh3)[names(pickupneigh3) == "df.X"] <- "X"
  df <-merge(df,pickupneigh3, by.x = 'X',by.y='X')
  rm(pickupneigh1)
  rm(pickupneigh2)
  rm(pickupneigh3)
  df$pickup_neighborhoods<- NULL
  return(df)
}

#encode & droppoffneigh
encode_droppoffneigh <- function(df){
  droppoffneigh1 <- data.frame(df$X, df$dropoff_neighborhoods)
  droppoffneigh2 <- dummyVars("~ .", data = droppoffneigh1)
  droppoffneigh3 <- data.frame(predict(droppoffneigh2, newdata = droppoffneigh1))
  names(droppoffneigh3)[names(droppoffneigh3) == "df.X"] <- "X"
  df <-merge(df,droppoffneigh3, by.x = 'X',by.y='X')
  rm(droppoffneigh1)
  rm(droppoffneigh2)
  rm(droppoffneigh3)
  df$dropoff_neighborhoods<- NULL
  return(df)
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
      ggtitle('Histogram of taxi rides fare amount') +
      theme_economist(base_size = 10, base_family = "sans", horizontal = TRUE, dkpanel = FALSE) +
      theme(text = element_text(family = "'NimbusSan'"), #, color = "grey20"
            plot.title = element_text(size=15),
            axis.title.y = element_text(size=10),
            axis.title.x = element_text(size=10),
            axis.text.x = element_text(angle = 45),
            legend.spacing.x = unit(0.5, 'cm')
      )
  }
  else if (graph == 'Time vs price'){
    ggplot(df, aes(y = fare_per_km, x = pickup_hour, group = pickup_hour) )+
      geom_boxplot() + 
      scale_x_continuous('Pick up hour') + 
      scale_y_continuous('Fare per kilometer amount') +
      ggtitle('Taxi fare per kilometer by hour') +
      theme_gray(base_size = 12, base_family = "sans") +
      #theme_hc(base_size = 10, base_family = "sans") +
      theme(text = element_text(family = "'NimbusSan'"), #, color = "grey20"
            plot.title = element_text(size=15),
            axis.title.y = element_text(size=10),
            axis.title.x = element_text(size=10),
            axis.text.x = element_text(angle = 45),
            legend.spacing.x = unit(0.5, 'cm')
      )
  }else if (graph == 'Count taxis by hour'){
    occ <- df %>% count(df$pickup_hour)
    print(occ)
    
    ggplot(occ, aes(y = n, x = `df$pickup_hour`, group = 2) )+
      geom_line() +
      scale_x_continuous('Pick up hour') + 
      scale_y_continuous('Taxi count') +
      ggtitle('Taxi count by hour') +
      theme_economist(base_size = 10, base_family = "sans", horizontal = TRUE, dkpanel = FALSE) +
      theme(text = element_text(family = "'NimbusSan'"), #, color = "grey20"
            plot.title = element_text(size=15),
            axis.title.y = element_text(size=10),
            axis.title.x = element_text(size=10),
            axis.text.x = element_text(angle = 45),
            legend.spacing.x = unit(0.5, 'cm')
      )
  }
  else if (graph == 'Evolution of prices'){
    sub <- select(df, pickup_date, fare_per_km)
    sub$year <- year(sub$pickup_date)
    sub$month <- month(sub$pickup_date)
    sub <- select(sub, year, month, fare_per_km)
    
    occ <- sub %>% 
      group_by(year, month) %>% 
      summarise(avg = mean(fare_per_km)) 
    
    ggplot(occ, aes(y = avg, x = month, group = 2) )+
      geom_line() +
      scale_x_continuous('Month') + 
      scale_y_continuous('Average price per km') +
      ggtitle('Evolution of prices') +
      theme_economist(base_size = 10, base_family = "sans", horizontal = TRUE, dkpanel = FALSE) +
      theme(text = element_text(family = "'NimbusSan'"), #, color = "grey20"
            plot.title = element_text(size=15),
            axis.title.y = element_text(size=10),
            axis.title.x = element_text(size=10),
            axis.text.x = element_text(angle = 45),
            legend.spacing.x = unit(0.5, 'cm') )
    
    
  }
  else if (graph == 'Histogram of Euclidean distance') {
    ggplot(df, aes(x=distance_kms)) + geom_histogram(binwidth=1)+
      scale_x_continuous(name="Distance in kms", limits=c(0, 50)) +
      ggtitle('Histogram of Euclidean distance')
  }
  else if (graph == 'Scatter Plot Distance Fare Amount' ) {
    ggplot(df, aes(x=distance_kms, y=fare_amount)) + geom_point() +
      geom_smooth(method="loess", se=F)
  }
  else if (graph == 'Fare per km plot')  {
    ggplot(df, aes(x=fare_per_km)) + geom_histogram(binwidth=0.5)+
      scale_x_continuous(name="Fare per km", limits=c(0, 100)) +
      ggtitle('Fare per km plot');
  }
  else if (graph == 'Average price per km')  {
    ggplot(df, aes(x=distance_kms, y=fare_per_km)) + geom_point() + ggtitle('Average price per km')
    
  }
  else if (graph == 'Marginal representation avg fare distance') {
    g <- ggplot(df, aes(distance_kms, fare_per_km)) + geom_count() 
    ggMarginal(g, type = "histogram", fill="transparent")
  }
  else if (graph == 'Results, scatter_plot_true_vs_preds'){
    ggplot(df, aes(x = as.numeric(true_val), y = as.numeric(Predictions))) +
      geom_point() + 
      scale_x_continuous('True value') + 
      scale_y_continuous('Predictions') + 
      ggtitle('Prediction Results') + 
      geom_smooth(method = lm)
  }
  else if (graph == 'Results, importance of features'){
    df <- df[order(-Gain),]
    positions <- df[, Feature]
    ggplot(df, aes(x = Feature, y = Gain)) +
      geom_bar(stat='identity') +
      coord_flip() +
      ggtitle('Feature importance') +
      scale_x_discrete(limits = positions)
  }
}

cross_validation <- function(df, model, formula, model_type, nfolds=10, boost_type = 'fare_per_km') {
  if(model_type == 'regression'){
    test.rmse <- rep(0, nfolds)
    test.mse <- rep(0, nfolds)
  }
  df <- df[sample(nrow(df)), ]
  index <- rep(1:nfolds, length.out = nrow(df))
  
  sum_rmse = 0
  max_rmse = 0
  min_rmse = 100                         # random variable for initialisation 
  for(i in 1:nfolds){
    insample  = which(index != i)
    outsample = which(index == i)
    
    X_train <- df[insample, ]
    X_test  <- df[outsample, ]
    
    if (model == 'XGBoost' && boost_type == 'fare_per_km') {
      labels =  X_train$fare_per_km
      ind_var = X_train$distance_kms
      XGB_regression <- xgb.DMatrix(data = as.matrix(ind_var),label = labels)
    }
    else if (model == 'XGBoost' & boost_type != 'fare_per_km'){
      labels = as.numeric(X_train$fare_amount)
      ind_var = select(X_train, 'pickup_latitude', 'pickup_longitude', 'month_feature', 'passenger_count', 'pickup_hour',
                                                'week_within_month', 'distance_kms', 'countdown', 'spring', 'summer', 'winter',  'df.weekday.Friday', 
                                                'df.weekday.Monday',  'df.weekday.Saturday', 'df.weekday.Sunday',  'df.weekday.Thursday', 
                                                'df.weekday.Tuesday', 'df.weekday.Wednesday', 'dropoff_longitude', 'dropoff_latitude', 'workday', 'month_feature', 'holiday')
   
      XGB_regression <- xgb.DMatrix(data = as.matrix(sapply(ind_var, as.numeric)), label = labels)
      
      
      target =  as.numeric(X_test$fare_amount)
      test_ind_var = select(X_test, 'pickup_latitude', 'pickup_longitude', 'month_feature',  'passenger_count', 'pickup_hour',
                                                   'week_within_month', 'distance_kms', 'countdown', 'spring', 'summer', 'winter',  'df.weekday.Friday', 
                                                   'df.weekday.Monday',  'df.weekday.Saturday', 'df.weekday.Sunday',  'df.weekday.Thursday', 'df.weekday.Tuesday', 'df.weekday.Wednesday', 
                            'dropoff_longitude', 'dropoff_latitude', 'workday', 'month_feature', 'holiday')
      X_test_cor_form = X_test
      X_test = xgb.DMatrix(data = as.matrix(sapply(test_ind_var, as.numeric)))
      
      params <- list(booster = "gbtree", objective = "reg:squarederror", eta=0.4, gamma=0.1, max_depth=15, min_child_weight=1, 
                     subsample=1, colsample_bytree=1)  
      amount_model <- xgb.train (params = params, data = XGB_regression, nrounds = 11, 
                                 print_every_n = 5,maximize = T , eval_metric = "rmse") 
    }
    
    if(model == 'randomForest'){
      amount_model <- randomForest(formula, X_train, ntree = 10, na.action = na.omit)
    }else if (model == 'tree'){
      amount_model <- tree(formula, data = X_train)
    }else if (model == 'lm'){
      amount_model <- lm(formula, data = X_train)
      target <- X_test$fare_amount
    }else if (model == 'XGBoost' && boost_type == 'fare_per_km'){
      print('in correct boost 2')
      params <- list(booster = "gbtree", objective = "reg:squarederror", eta=0.3, gamma=0, max_depth=9, min_child_weight=1, 
                     subsample=1, colsample_bytree=1)  
      amount_model <- xgb.train (params = params, data = XGB_regression, nrounds = 11, 
                                 print_every_n = 5,maximize = T , eval_metric = "rmse") 
      target =  X_test$fare_per_km
      test_ind_var = X_test$distance_kms
      X_test_cor_form = X_test
      X_test = xgb.DMatrix(data = as.matrix(test_ind_var))
    }

    predictions  <- predict(amount_model, X_test)
    
    if(model_type == 'classifier') {
      target <- X_test$fare_amount_class
      
      cm <- confusionMatrix(target, predictions)
      print((amount_model$confusion))
      print(cm$overall)
    }else {
      cat('evals')
      if (model == 'XGBoost') {        # X_test is xgbmatrix and target variable is amount per_km
        target = target
      }
      else {
        target <- X_test$fare_amount
      }
      test.rmse[i] =  RMSE(target, predictions)
      test.mse[i] =  mean((target -  predictions)^2)
      cat(c('RMSE: ', test.rmse[i], ' MSE: ', test.mse[i], '\n'))
      sum_rmse =  sum_rmse + test.rmse[i]
      if (i == 1) {
        min = test.rmse[i]                    # min initialisation
      }
      if (test.rmse[i] < min_rmse) {
        min_rmse = test.rmse[i]
      }
      if (test.rmse[i] > max_rmse) {
        max_rmse = test.rmse[i]
      }
    }
  }
  if (model == 'XGBoost') {
  avg_rmse = sum_rmse / nfolds
  cat(avg_rmse, min_rmse, max_rmse)
  }
  
  if(model == 'XGBoost'){
    preds_df <- data.frame('Predictions' = predictions, 'true_val'= target)
  }else{
    preds_df <- data.frame('Predictions' = predictions, 'true_val'= X_test$fare_amount)
  }
  
  return(preds_df)
}


final_predictions <- function(X_train, X_test, features){
  # format data 
  labels = as.numeric(X_train$fare_amount)
  ind_var = select(X_train, features)
  
  XGB_regression <- xgb.DMatrix(data = as.matrix(sapply(ind_var, as.numeric)), label = labels)
  
  target =  as.numeric(X_test$fare_amount)
  test_ind_var = select(X_test, features)
  
  X_test_cor_form = X_test
  X_test = xgb.DMatrix(data = as.matrix(sapply(test_ind_var, as.numeric)))
  
  
  # declare model 
  params <- list(booster = "gbtree", objective = "reg:squarederror", eta=1, gamma=0.05, max_depth=7, 
                 subsample=1, colsample_bytree=1)  

  amount_model <- xgb.train (params = params, data = XGB_regression, nrounds = 50, 
                             print_every_n = 5,maximize = T , eval_metric = "rmse") 
  
  # get the importance of features
  importance <- xgb.importance(feature_names = features, model = amount_model)
  #head(importance)
  #xgb.plot.importance(importance_matrix = importance)
  
  
  # predicy 
  predictions  <- predict(amount_model, X_test)
  
  rmse <- RMSE(target, predictions)
  mse <- mean((target -  predictions)^2)
  cat(c('RMSE: ', rmse, ' MSE: ', mse, '\n'))
  
  preds_df <- data.frame('Predictions' = predictions, 'true_val'= target)
  return(list('preds_df' = preds_df, 'feature_importance' = importance))
}



estimate_fare_per_km_pred_cv <- function(df, model, nfolds=10) {
  
  df <- df[sample(nrow(df)), ]
  index <- rep(1:nfolds, length.out = nrow(df))
  
  for(i in 1:nfolds){
    insample  = which(index != i)
    outsample = which(index == i)
    
    X_train <- df[insample, ]
    X_test  <- df[outsample, ]
    
    if (model == 'XGBoost') {
      labels =  X_train$fare_per_km
      ind_var = X_train$distance_kms
      XGB_regression <- xgb.DMatrix(data = as.matrix(ind_var),label = labels)
    }
    
    if (model == 'XGBoost'){
      params <- list(booster = "gbtree", objective = "reg:squarederror", eta=0.3, gamma=0, max_depth=9, min_child_weight=1, 
                     subsample=1, colsample_bytree=1)  
      amount_model <- xgb.train (params = params, data = XGB_regression, nrounds = 11, 
                                 print_every_n = 5,maximize = T , eval_metric = "rmse") 
      target =  X_test$fare_per_km
      test_ind_var = X_test$distance_kms
      X_test_cor_form = X_test
      X_test = xgb.DMatrix(data = as.matrix(test_ind_var))
    }
    
    predictions  <- predict(amount_model, X_test)
    df[outsample,]$fare_per_km_pred <- predictions
    
  }
  
  return(df)
}


# eta = 0.3 , max_depth=9


xgb_prediction <- function (X_train, X_test, features, target){
  # format data 
  # labels = as.numeric(X_train$fare_amount)
  label_train =  select(X_train, target)
  ind_var_train = select(X_train, features)
  
  XGB_regression <- xgb.DMatrix(data = as.matrix(sapply(ind_var_train, as.numeric)), label = as.matrix(sapply(label_train, as.numeric)))
  
  params <- list(booster = "gbtree", objective = "reg:squarederror", eta=0.3, gamma=0, max_depth=9, min_child_weight=1, 
                 subsample=1, colsample_bytree=1)  
  amount_model <- xgb.train (params = params, data = XGB_regression, nrounds = 11, 
                             print_every_n = 5,maximize = T , eval_metric = "rmse") 
  
  label_test =  select(X_test, target)
  ind_var_test =select(X_test, features)
  Test_input_data = xgb.DMatrix(data =as.matrix(sapply(ind_var_test, as.numeric)))
  
  predictions  <- predict(amount_model, Test_input_data)
  
  test.rmse =  RMSE(as.matrix(label_test), predictions)
  test.mse =  mean((as.matrix(label_test) -  predictions)^2)
  cat(c('RMSE: ', test.rmse, ' MSE: ', test.mse, '\n'))
  
  importance <- xgb.importance(feature_names = features, model = amount_model)
  
  return(predictions)
  # return(importance)
  
}

xgb_classification <- function (X_train, X_test, features, target){
  # format data 
  # labels = as.numeric(X_train$fare_amount)
  label_train =  select(X_train, target)
  label_train = sapply(label_train, as.numeric) - 1
  ind_var_train = select(X_train, features)
  
  XGB_classification <- xgb.DMatrix(data = as.matrix(sapply(ind_var_train, as.numeric)), label = as.matrix(label_train))
  
  params1 <- list(booster = "gbtree", objective = "binary:logistic", 
                  eta=0.1, gamma=0, max_depth=7, min_child_weight=1, 
                  subsample=1, colsample_bytree=1)
  
  
  params <- list(booster = "gbtree", objective = "multi:softmax", num_class = 5, eta=0.1, gamma=0, max_depth=9, min_child_weight=1, 
                 subsample=1, colsample_bytree=1)  
  
  amount_model <- xgb.train (params = params, data = XGB_classification, nrounds = 9, 
                             print_every_n = 5,maximize = T , eval_metric = "auc") 
  
  label_test =  select(X_test, target)
  label_test =  sapply(label_test, as.numeric) - 1
  ind_var_test =select(X_test, features)
  Test_input_data = xgb.DMatrix(data =as.matrix(sapply(ind_var_test, as.numeric)))
  
  predictions  <- predict(amount_model, Test_input_data)
  
  confusionMatrix(factor(predictions), factor(label_test))
  
  return(predictions)
  
}

#------------------------------------------------------------------------------------------------

# set working dir to my file 
#setwd('C:/Users/Camille/Documents/GITHUB/MACHINE LEARNING/Big_Data_Analytics/NYC_taxi_fare/new-york-city-taxi-fare-prediction/')
# setwd("~/workspace/NYC_taxi_fare")
#train_df <- read.csv(file = './data/faresnew2014.csv')
setwd('C:/Users/kleok/OneDrive/Desktop/Master in DSBA/Semester 2/ESSEC/Big Data Analytics/kleo staff')
train_df <- read.csv(file = './faresnew2014.csv')
##read data, choose subset 
#train_df<-read.csv('/Users/thomasdorveaux/Desktop/Big Data Analytics/Group Assignment/NYC_taxi_fare/faresnew2014.csv')
#train_df <- read.csv(file = './data/data.csv') 
#head(train_df)

# Feature engineering ---------------------------------------------------------------------------
train_df <- remove_bad_rows(train_df)
train_df <- date_time_features(train_df)
train_df <- compute_distance(train_df)
train_df <- remove_zero_distance(train_df)
train_df <- get_day_of_week(train_df)
train_df <- round_to_hour(train_df)
train_df <- worday_weekend_feature(train_df)
train_df <- get_month_day_feature(train_df)
train_df <- week_within_month(train_df)

train_df <- get_season_feature(train_df)
#train_df <- log_amount(train_df)
train_df <- get_only_manhattan_data(train_df)
#train_df <- airport_start_or_dest(train_df)
train_df <- classify_fare_amount(train_df)
train_df <- compute_fare_per_km(train_df)
train_df <- remove_large_fare_per_km(train_df, 20)
train_df <- add_NY_holidays(train_df)


# needed only for countdown
ny_holidays_2014_2015 = c('2014-01-01', 
                          '2014-01-20', '2014-02-12', '2014-02-17', '2014-05-26', '2014-07-04',
                          '2014-09-01', '2014-11-11', '2014-11-27', '2014-12-25', '2015-01-01',
                          '2015-01-19', '2015-02-12', '2015-02-16', '2015-03-25')




train_df$countdown = 0     # initialise new column
for(j in 1:length(ny_holidays_2014_2015)){
  train_df = countdown_holidays(train_df, ny_holidays_2014_2015[j])
}

setwd('C:/Users/kleok/OneDrive/Desktop/Master in DSBA/Semester 2/ESSEC/Big Data Analytics/NYC_taxi_fare')
train_df <- get_neighborhood(train_df)

# One-hot Encoding
train_df <- encode_weekday(train_df)
train_df <- encode_season(train_df)
train_df <- encode_pickupneigh(train_df)
train_df <- encode_droppoffneigh(train_df)

# Remove redundant columns
train_df <- train_df[, !names(train_df) %in% c('key', 'pickup_datetime')]

# separate test and train dataset
test_df <- train_df[(as.Date('2015-01-01') <= train_df$pickup_date) & (train_df$pickup_date <= as.Date('2015-04-01')), ]
train_df <- train_df[train_df$pickup_date < as.Date('2015-01-01'), ]
head(train_df)
head(test_df)

## PLOTTING GRAPHS --------------------------------------------------------------------------------

# specify which graph you want to plot if any
#plot_graphs(train_df, graph = 'fare_amount_hist')
#plot_graphs(train_df, graph = 'Histogram of Euclidean distance')
#plot_graphs(train_df, graph = 'Scatter Plot Distance Fare Amount')
#plot_graphs(train_df, graph = 'Fare per km plot')
#plot_graphs(train_df, graph = 'Average price per km')
#plot_graphs(train_df, graph = 'Marginal representation avg fare distance')

################ MODEL A: Distance -> fare/km -> total_fare  #######################

#train_df$fare_per_km_pred = 0 
#train_df = estimate_fare_per_km_pred_cv(train_df, 'XGBoost')
#total_fare_pred(train_df,'XGBoost')


## CROSS VALIDATION ON AVG_FARE_PER_KM ------------------------------------------------------------
#cross_validation(train_df, 'XGBoost', 'no_formula' , 'regression',  boost_type = 'fare_per_km')

#regression_fare_per_km_formula = fare_per_km ~ distance_kms
#classifier_amount_formula = fare_amount_class ~ pickup_latitude + pickup_longitude + month_feature +  weekday +
#  dropoff_longitude + dropoff_latitude + passenger_count + pickup_hour

#cross_validation(train_df, 'randomForest', regression_fare_per_km_formula, 'regression')


#fare_per_km_formula = fare_per_km ~ pickup_latitude + pickup_longitude +
#  dropoff_longitude + dropoff_latitude + passenger_count + distance_kms

#cross_validation(train_df, 'randomForest', fare_per_km_formula, 'regression')

#cross_validation(train_df, 'lm', fare_per_km_formula, 'regression')


## CROSS VALIDATION ON AMOUNT MODEL ---------------------------------------------------------------

# train_df=train_df %>% mutate_if(is.character, as.numeric)
#regression_amount_formula = fare_amount ~ pickup_latitude + pickup_longitude + month_feature +  weekday +
#  dropoff_longitude + dropoff_latitude + passenger_count + pickup_hour + workday +month_feature + day_feature + week_within_month +season_feature - 
#  fare_amount_class - fare_per_km + countdown

## CROSS VALIDATION ON AMOUNT MODEL ---------------------------------------------------------------
#col_names = names(train_df)
# col_names[3:length(col_names)]
#train_df2 = select(train_df, col_names[3:length(col_names)])

#train_df %>% select(fare_amount, distance_kms, month_feature)

#regression_amount_formula = fare_amount ~ pickup_latitude + pickup_longitude + month_feature +  weekday +
#  dropoff_longitude + dropoff_latitude + passenger_count + pickup_hour

#regression_amount_formula = log(fare_amount) ~ pickup_latitude + pickup_longitude + month_feature + weekday + 
#  passenger_count + pickup_hour + workday  + week_within_month + season_feature+ distance_kms

#classifier_amount_formula = fare_amount_class ~ pickup_latitude + pickup_longitude + month_feature +  weekday +
#  dropoff_longitude + dropoff_latitude + passenger_count + pickup_hour + distance_kms

#cross_validation(train_df, 'randomForest', classifier_amount_formula, 'classifier')
#preds_df <- cross_validation(train_df, 'randomForest', regression_amount_formula, 'regression')
#preds_df <- cross_validation(train_df, 'lm', regression_amount_formula, 'regression')
#cross_validation(train_df, 'tree', regression_amount_formula, 'regression')

# Model on everything
#preds_df <- cross_validation(train_df, 'XGBoost', 'no_formula' , 'regression', boost_type = 'second_type')

### FINAL PREDICTIONS FOR BASIC MODEL -------------------------------------------------------------------------------------------

features <- c('pickup_latitude', 'pickup_longitude', 'dropoff_latitude', 'dropoff_longitude',   'month_feature', 'passenger_count', 'pickup_hour',
'week_within_month', 'distance_kms', 'countdown', 'spring', 'summer', 'winter',  'df.weekday.Friday', 
'df.weekday.Monday',  'df.weekday.Saturday', 'df.weekday.Sunday',  'df.weekday.Thursday', 
'df.weekday.Tuesday', 'df.weekday.Wednesday', 'workday', 'month_feature', 'holiday')

output <- final_predictions(train_df, test_df, features)


## PLOTTING RESULTS --------------------------------------------------------------------------------------------------------------
plot_graphs(output$preds_df, 'Results, scatter_plot_true_vs_preds')
plot_graphs(output$feature_importance, 'Results, importance of features')


plot(roc(output$preds_df, lin_mod), print.auc = TRUE)


####################### 2 Level Regression Model  ###############################

# Find fare_per_km with cross validation both for train set and test set
train_df$fare_per_km_pred = 0
train_df = estimate_fare_per_km_pred_cv(train_df, 'XGBoost')
test_df$fare_per_km_pred = 0
test_df = estimate_fare_per_km_pred_cv(test_df, 'XGBoost')

col_names = names(train_df)
features = col_names[-c(1,2,8,9,10, 16,17,18,21)]     # to drop_off_neighbors remain, so have to be removed (21)
                                                      # 17 do not need it because of  fare_per_km_pred (33)
                                                      # feature 10 not used, already used in first regression
target  = "fare_amount"

h = xgb_prediction(train_df, test_df, features, target)
# RMSE:  2.23177740287931  MSE:  4.98083037600272
#################### Plot true -prediction graph ########################
output <- data.frame(matrix(unlist(h), nrow=length(h), byrow=T),stringsAsFactors=FALSE)
output$true_val = test_df$fare_amount
output$Predictions = h
output = select(output, names(output)[-c(1)])
plot_graphs(output,'Results, scatter_plot_true_vs_preds')


##################### Model: Classification -> Regeression ##################
#train_df = select(train_df, names(train_df)[-c(409)])
#test_df = select(test_df, names(train_df)[-c(409)])


col_names = names(train_df)
features = col_names[-c(1,2,8, 9, 10,16, 17, 18, 21)]         # 10 for second round, 16 is the target (do not include)
                                                                    # 409 added from previous model
target = "fare_amount_class"

pred_fare_amount_class = xgb_classification(train_df, test_df, features, target)


test_df$fare_amount_class = pred_fare_amount_class
train_df$fare_amount_class = sapply(train_df$fare_amount_class, as.numeric) - 1    # bring it to the same format

# 10  
features = col_names[-c(1,2,8, 9,17,18, 21)]    # 16 einai auto pou ipologises, kai distance_kms to thelw edw (10), oxi 21 encoding
                                                       
target = "fare_amount"
h = xgb_prediction(train_df, test_df, features, target)

#RMSE:  2.49505321499959  MSE:  6.22529054567981   # by keeping fare_per_km prediction from previous model
# RMSE:  2.21752144183221  MSE:  4.91740134498559 
# Plot true predicted graph
output <- data.frame(matrix(unlist(h), nrow=length(h), byrow=T),stringsAsFactors=FALSE)
output$true_val = test_df$fare_amount
output$Predictions = h
output = select(output, names(output)[-c(1)])
plot_graphs(output,'Results, scatter_plot_true_vs_preds')

