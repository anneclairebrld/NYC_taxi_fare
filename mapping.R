
# Libraries ------------------------------------------------------------------

library(readr)
library(dplyr)
library(geosphere)
library(ggplot2)
library(lubridate)
library(tmap)
library(leaflet)
library(data.table)

require(devtools) # loading required package: devtools
devtools::install_github("rCarto/osrm")
library(osrm)
library(cartography)
library(rgdal) # coordinate conversion 

getOption("osrm.server")

# set working directory ------------------------------------------------------

setwd('C:/Users/Camille/Documents/GITHUB/MACHINE LEARNING/Big_Data_Analytics/
      NYC_taxi_fare')

# use data.table to read all the lines
# file = fread('./new-york-city-taxi-fare-prediction/data/train.csv', 
#               sep = ",")

# Read training data ---------------------------------------------------------

df <- read.csv(file = './new-york-city-taxi-fare-prediction/data/train.csv', 
               nrows=100000)
df <- cbind(transaction = row.names(df), df)

# re-format date and time
df$pickup_date = as.Date(df$pickup_datetime)
df$pickup_time = format(as.POSIXct(df$pickup_datetime), '%H:%M:%S') #NOT WORKING

# Routing --------------------------------------------------------------------

data = df[1,c("transaction", "pickup_longitude","pickup_latitude",
              "dropoff_latitude", "dropoff_longitude")]

# set existing coordinates as lat-long system
cord_pickup = SpatialPoints(
              cbind(data$pickup_longitude, -data$pickup_latitude), 
              proj4string = CRS("+proj=longlat"))

# set existing coordinates as lat-long system
cord_dropoff =  SpatialPoints(
                cbind(data$dropoff_longitude, -data$dropoff_latitude), 
                proj4string = CRS("+proj=longlat"))

cord_pickup.UTM <- spTransform(cord_pickup, CRS("+init=epsg:4326"))
cord_dropoff.UTM <- spTransform(cord_dropoff, CRS("+init=epsg:4326"))

# dataframes
df_pickup <- data.frame(data$transaction, cord_pickup.UTM)
colnames(df_pickup) <- c("id", "long", "lat")
df_dropoff <- data.frame(data$transaction, cord_pickup.UTM)
colnames(df_dropoff) <- c("id", "long", "lat")

# vectors
vec_pickup = c(cord_pickup.UTM)
vec_dropoff = c(cord_pickup.UTM)


# osrmTable(dfo = df_pickup, 
#                       ido = df_pickup$id, 
#                       xo = df_pickup$long, 
#                       yo = df_pickup$lat, 
#                       dfd = df_dropoff, 
#                       idd = df_dropoff$id, 
#                       xd = df_dropoff$long, 
#                       yd = df_dropoff$lat, 
#                       limit = 100)

osrmRoute(df_pickup, df_dropoff, loc = data, overview = "simplified", exclude = NULL)
                       
                       
write.table(distancias, file = "matriz.txt", sep="\t")

