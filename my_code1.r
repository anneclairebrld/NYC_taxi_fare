install.packages('devtools')
devtools::install_github("dkahle/ggmap")
install.packages('viridis')
install.packages('tree')
install.packages('tree')
install.packages('randomForest')

install.packages('tidyverse')

library(tidyverse)
library(dplyr)
library(ggplot2)

# Reading in the taxi data
# read only first 100 lines as too much data 
file_in <-file('./train.csv', 'r')
chunk_size <- 1000
taxi <- read.csv(file = './train.csv', nrows=100000) 

# Taking a look at the first couple of rows in taxi
head(taxi)

# Cleaning taxi data
taxi <- taxi %>%
  filter(fare_amount > 0 );

head(taxi)

# Zooming in Manhattan
taxi <- taxi  %>% filter( between(pickup_latitude, 40.70, 40.83) & between(pickup_longitude, -74.025, -73.93) )
taxi <- taxi  %>% filter( between(dropoff_latitude, 40.70, 40.83) & between(dropoff_latitude, -74.025, -73.93) )

# Drawing s map
install.packages('ggmap')
library(ggmap)
install.packages('viridis')
library(viridis)

qmap(location = "boston university") 

my_key='MY_KEY_EXTRACTED_FROM_GOOGLE_DEV_ACCOUNT'
register_google(key = my_key)
manhattan <- get_map("manhattan", zoom = 12, color = "bw")
