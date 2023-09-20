# for each ndvi raster, get all past fires, then create a raster of last fire date
library('dplyr')     # for data wrangling
library('purrr')     # for functional programming
library('sf')        # for spatial data
library('terra')     # for working with rasters

# function to find cumulative burn
find_prop <- function(.date, shp, mask) {
  f <- filter(shp, date <= .date)
  
  burned <- map(1:nrow(f), \(.i) {
    .r <- slice(f, .i) %>%
      rasterize(r, cover = TRUE, background = 0)
    
    names(.r) <- st_drop_geometry(f$date[.i])
    return(.r)
  }) %>%
    rast() %>%
    max() %>%
    mask(mask)
  
  names(burned) <- .date # add date as layer name
  
  return(burned)
}

# test the function
if(FALSE) {
  r <- rast('data/ndvi-rasters/study-area-albers/VI_16Days_250m_v61/NDVI/MOD13Q1_NDVI_2000_049.tif')
  values(r) <- 1
  
  fires <- st_read('data/fires/study-fires.shp')
  area <- st_read('data/study-area/study-area-albers.shp')
  
  plot(c(find_prop(as.Date('1925-01-01'), shp = fires, mask = area),
         find_prop(as.Date('1926-01-01'), shp = fires, mask = area),
         find_prop(as.Date('1927-01-01'), shp = fires, mask = area),
         find_prop(as.Date('1928-01-01'), shp = fires, mask = area),
         find_prop(as.Date('1929-01-01'), shp = fires, mask = area),
         find_prop(as.Date('1930-01-01'), shp = fires, mask = area),
         find_prop(as.Date('1931-01-01'), shp = fires, mask = area),
         find_prop(as.Date('1932-01-01'), shp = fires, mask = area)))
}
