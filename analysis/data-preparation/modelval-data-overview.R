#This file is going to filter the data to the model validation study site

#import packages
library('dplyr')     # for data wrangling
library('tidyr')     # for data wrangling
library('purrr')     # for functional programming
library('sf')        # for spatial data
library('terra')     # for working with rasters
library('lubridate') # for working with dates
library('ggplot2')   # for fancy plots (checking data at the end)
source('functions/find_prop.R') # to find proportioned burned
source('functions/find_dates.R') # to find date of last fire

mvarea <- st_read('data/model-validation-area/model_validation_area.shp')


d <- readRDS('data/labelled-ndvi-data.rds') 

#since the filtered data already exists,I will simply filter it to the boundary of the mv study area shp

df_sf <- st_as_sf(d, coords = c("x_alb", "y_alb"), crs = 3005) #d needed to be of class "sf" in order to use st_intersection()

filtered_data <- st_intersection(df_sf, mvarea)

#save as an rds
saveRDS(filtered_data, 'data/model-val-filtered-data.rds')
