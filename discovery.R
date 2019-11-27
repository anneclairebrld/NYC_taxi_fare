library(readr)
library(dplyr)
library(rgdal)
install.packages('geosphere')
library(geosphere)
library(ggplot2)
install.packages('lubridate')
library(lubridate)
library(tmap)
library(data.table)
library(disk.frame)

setup_disk.frame()
options(future.globals.maxSize = Inf)
table <- csv_to_disk.frame('./data/train.csv')

setwd('~/workspace/NYC_taxi_fare') # set this as working directory

# read only first 100 lines as too much data 

table <- fread('./data/train.csv',  nrows=10)

file_in <-file('./data/train.csv', 'r')
chunk_size <- 1000
df <- read.csv(file = './data/train.csv', nrows=50000) 

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
df <- subset(df, !df$pickup_latitude < -90)
df <- subset(df, !df$dropoff_longitude < -180)
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

sub_df <- subset(df, year(df$pickup_date) == 2011) 
occ <- sub_df %>% count(sub_df$day_of_week, sub_df$pickup_hour)
head(occ)

occ$`sub_df$day_of_week` <- factor(occ$`sub_df$day_of_week`, levels=c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'),  labels=c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'))
# plot number of taxis ordered for each
ggplot(data = occ, aes(x = occ$`sub_df$pickup_hour`, group = occ$`sub_df$day_of_week`, y=occ$n)) +
  geom_point(aes(colour = factor(occ$`sub_df$day_of_week`))) +
  geom_line(aes(colour = factor(occ$`sub_df$day_of_week`))) +
  scale_color_discrete('Weekday') +
  scale_x_discrete('Hour of the day', limits = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23)) +
  scale_y_continuous('Number of taxi rides') +
  ggtitle('Number of rides by hour of the day across weekdays (year 2015)')



library(sp)
library(st)
library(sf)
library(raster)
washington <- st_read('./data/washington.geojson')

pickup_points <- df_pickup %>% st_as_sf(coords = c('pickup_longitude', 'pickup_latitude'), crs = proj4)
pickup_points_sf = st_as_sf(df_pickup)
plot(pickup_points_sf)

tm_shape(washington) +
  tm_polygons('#f0f0f0f0', border.alpha = 0.2) +
  tm_shape(pickup_points_sf) +
  tm_dots(size = 0.5, alpha = 0.3)

points <- 
qtm('washington') + tm_dots(df_pickup)
# remove unknown pickups with unknown/unfeasible locations 
df_pickup = df %>% select(pickup_latitude, pickup_longitude) 
coordinates(df_pickup) <- ~ pickup_longitude + pickup_latitude # convert to spatialpointsdf Importantstep  => remembers the coordinates for each 
proj4 <- st_crs(washington)$proj4string
pickup_points_sf = df_pickup %>% st_as_sf(coors=c('pickup_longitude', 'pickup_latitude'),  crs = proj4)
# pickup_points_rastered = raster(pickup_points_sf)
tm_shape(washington) +
  tm_polygons('#f0f0f0f0', border.alpha = 0.2) +
  tm_dots(c=pickup_points) 

#map('county', 'washington')
map <- set_projection(df_pickup, current.projection = 'longlat', projection = 27700)

print(washington$geometry)
print(df_pickup)
## pick up airport 
## heat map prices-> normalised the pice with the distance 
## heat map  

