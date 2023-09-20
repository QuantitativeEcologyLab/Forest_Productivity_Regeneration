library('dplyr')     # for data wrangling
library('purrr')     # for functional programming
library('sf')        # for spatial data
library('terra')     # for working with rasters

find_dates <- function(.date, shp, mask) {
  s <- filter(shp, date <= .date) # only keep fires or cuts prior to .date
  
  .raster <-
    map(1:nrow(s), \(.i) { # for each fire or cut
      .r <- slice(s, .i) %>% # only take the last fire or cut
        rasterize(r, cover = TRUE, background = NA) # find proportion
      
      values(.r) <- if_else(! is.na(values(.r)), # add fire dates
                            lubridate::decimal_date(s[.i, ]$date),
                            NA_real_)
      return(.r)
    }) %>%
    rast() %>% # turn list into raster stack
    max(na.rm = TRUE) %>% # take the latest burn date for each cell
    mask(mask) # drop areas outside study area shapefile
  
  names(.raster) <- .date # specify the reference date as the layer name
  return(.raster)
}

# test the function
if(FALSE) {
  r <- rast('data/ndvi-rasters/study-area-albers/VI_16Days_250m_v61/NDVI/MOD13Q1_NDVI_2000_049.tif')
  values(r) <- 1
  
  fires <- st_read('data/fires/study-fires.shp')
  area <- st_read('data/study-area/study-area-albers.shp')
  
  plot(c(find_dates(as.Date('1925-01-01'), shp = fires, mask = area),
         find_dates(as.Date('1926-01-01'), shp = fires, mask = area),
         find_dates(as.Date('1927-01-01'), shp = fires, mask = area),
         find_dates(as.Date('1928-01-01'), shp = fires, mask = area),
         find_dates(as.Date('1929-01-01'), shp = fires, mask = area),
         find_dates(as.Date('1930-01-01'), shp = fires, mask = area)))
}
