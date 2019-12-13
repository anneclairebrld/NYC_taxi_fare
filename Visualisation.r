library(readr)
library(dplyr)
#install.packages('geosphere')
library(geosphere)
library(ggplot2)
library(lubridate)

install.packages("Hmisc")
library(Hmisc)

#install.packages("data.table")
library("data.table")

# too time consuming - have not done it yet
input <- if (file.exists("train.csv")) {
  "train.csv"	
}

# use data tables 
train <- fread(input)   # gia na xrisimopoiisoume data tables

setwd('C:/Users/kleok/OneDrive/Desktop/Master in DSBA/Semester 2/ESSEC/Big Data Analytics/NYC_taxi_fare') # set this as working directory

# read only first 100 lines as too much data 
file_in <-file('./train.csv', 'r')
chunk_size <- 1000
# Hello 


df <- read.csv(file = './train.csv', nrows=100000) 

#reformat date and time
df$pickup_date <- as.Date(df$pickup_datetime)
df$pickup_time <- format(as.POSIXct(df$pickup_datetime), '%H:%M:%S')

# remove redundant columns
df <- df[, !names(df) %in% c('key', 'pickup_datetime')]
head(df)



df_descr = describe(df)

c(1:5,10)

head(df[1])
df[df$fare_amount>10]

head(df)

min(df$pickup_date)

newdata <- subset(df, month(df$pickup_date) >= 1 | year(df$pickup_date) >= 2015)


## % choose the last 6 months
newdata <- subset(df, year(df$pickup_date) >= 2015 & month(df$pickup_date)>=3 )

newdata$week = 0
head(newdata)
newdata$day_of_week <- weekdays(as.POSIXct(newdata$pickup_date), abbreviate = F)
newdata$week <- cut.Date(newdata$pickup_date, breaks = "1 week", labels = FALSE)
newdata$pickup_hour[df$pickup_hour == 24] <- 0

newdata$

## Let's do some plots

ggplot(data = occ, aes(x = occ$`sub_df$pickup_hour`, group = occ$`sub_df$day_of_week`, y=occ$n)) +
  geom_point(aes(colour = factor(occ$`sub_df$day_of_week`))) +
  geom_line(aes(colour = factor(occ$`sub_df$day_of_week`))) +
  scale_color_discrete('Weekday', label=c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday')) +
  scale_x_discrete('Hour of the day', limits = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23)) +
  scale_y_continuous('Number of taxi rides') +
  ggtitle('Number of rides by hour of the day across weekdays (year 2015)')


## Plots

theme_set(theme_bw())
## Historgam of number of fares
ggplot(df, aes(x=fare_amount)) + geom_histogram(binwidth=1)+
  scale_x_continuous(name="Fare", limits=c(0, 100)) +
  ggtitle('Histogram of fares');
 

# Histogram of distance of ride

ggplot(df, aes(x=distance_euclidean)) + geom_histogram(binwidth=1)+
  scale_x_continuous(name="Euclidean Distance", limits=c(0, 50)) +
  ggtitle('Histogram of Euclidean distance');


# Scatter plot Euclidean distance - Fare + smoothing curve

ggplot(df, aes(x=distance_euclidean, y=fare_amount)) + geom_point() +
  geom_smooth(method="loess", se=F)

# Histogram of avg_price per mile (euclidean distance)
df$avg_fare <-df$fare_amount/df$distance_euclidean

ggplot(df, aes(x=avg_fare)) + geom_histogram(binwidth=0.5)+
  scale_x_continuous(name="Average fare", limits=c(0, 30)) +
  ggtitle('Average fare of euclidean distance');

# Scatter plot  of avg_price per euclidean_distance
ggplot(df, aes(x=distance_euclidean, y=avg_fare)) + geom_point()



# ggMarginal plot
library(ggplot2)
install.packages("ggExtra")
library(ggExtra)
  g <- ggplot(df, aes(distance_euclidean, avg_fare)) + 
    geom_count() + 
    geom_smooth(method="lm", se=F)
  
  ggMarginal(g, type = "histogram", fill="transparent")
  ggMarginal(g, type = "boxplot", fill="transparent")


# error gia non finite value