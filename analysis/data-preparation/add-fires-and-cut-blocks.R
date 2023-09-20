# for each ndvi raster, get all past fires, then create a raster of last fire date
library('dplyr')     # for data wrangling
library('tidyr')     # for data wrangling
library('purrr')     # for functional programming
library('sf')        # for spatial data
library('terra')     # for working with rasters
library('lubridate') # for working with dates
library('ggplot2')   # for fancy plots (checking data at the end)
source('functions/find_prop.R') # to find proportioned burned
source('functions/find_dates.R') # to find date of last fire
theme_set(theme_bw())

area <- st_read('data/study-area/study-area-albers.shp') # study area shp
fires <- st_read('data/fires/study-fires.shp') # fire shapefile
cuts <- st_read('data/cut-blocks/study-cut-blocks.shp') # cut block shp

# create a standard raster
r <- rast('data/ndvi-rasters/study-area-albers/VI_16Days_250m_v61/NDVI/MOD13Q1_NDVI_2000_049.tif') %>%
  `values<-`(1) %>%
  mask(area)
plot(r)

d <- tibble(
  file = list.files(
    path = 'data/ndvi-rasters/study-area-albers/VI_16Days_250m_v61/NDVI/',
    pattern = '.tif', full.names = TRUE)[c(1, 500)],
  date = substr(file,
                start = 74, stop = nchar(file) - nchar('.tif')) %>%
    as.Date(format = '%Y_%j'),
  ndvi = map2(file, date, \(.file, .date) {
    .r <- rast(.file) %>%
      mask(area) %>%
      project(r)
    names(.r) <- .date
    return(.r)
  }),
  last_fire = map(date, \(x) find_dates(x, shp = fires, mask = area)),
  prop_fire = map(date, \(x) find_prop(x, shp = fires, mask = area)),
  last_cut = map(date, \(x) find_dates(x, shp = cuts, mask = area)),
  prop_cut = map(date, \(x) find_prop(x, shp = cuts, mask = area))) %>%
  select(! file) # drop file column

# check rasters
plot(rast(d$ndvi)) # normalized difference vegetation index
plot(rast(d$last_fire)) # decimal date of last fire
plot(rast(d$prop_fire)) # proprtion of cell burned to date
plot(rast(d$last_cut)) # decimal date of last clear cut
plot(rast(d$prop_cut)) # proportion of cell clear cut to date

# convert all rasters to data frames
#' *NOTE:* some `NA` NDVI values require `na.rm = FALSE`, so it needs to be
#'         `FALSE` here, too.
raster_to_df <- function(.r, values_to) {
  as.data.frame(.r, xy = FALSE, na.rm = FALSE) %>%
    pivot_longer(! c(), values_to = values_to) %>%
    select(! name)
}

d_unnested <-
  d %>%
  mutate(ndvi = map(ndvi, \(x) {
    as.data.frame(x, xy = TRUE, na.rm = FALSE) %>%
      pivot_longer(! c(x, y), values_to = 'ndvi') %>%
      select(-name) # date column already exists
  }),
  last_fire = map(last_fire, \(x) raster_to_df(x, 'last_fire')),
  prop_fire = map(prop_fire, \(x) raster_to_df(x, 'prop_fire')),
  last_cut = map(last_cut, \(x) raster_to_df(x, 'last_cut')),
  prop_cut = map(prop_cut, \(x) raster_to_df(x, 'prop_cut'))) %>%
  unnest(ndvi:prop_cut) %>%
  filter(! is.na(ndvi))

if(FALSE) {
  d_small <- filter(d_unnested, date %in% unique(date)[1:2]) 
  cols <- rev(terrain.colors(10))
  
  ggplot(d_small, aes(x, y, fill = ndvi)) +
    coord_equal() +
    facet_wrap(~ date) +
    geom_raster() +
    scale_fill_gradientn(colors = cols)
  ggplot(d_small, aes(x, y, fill = last_fire)) +
    coord_equal() +
    facet_wrap(~ date) +
    geom_raster() +
    scale_fill_gradientn(colors = cols)
  ggplot(d_small, aes(x, y, fill = prop_fire)) +
    coord_equal() +
    facet_wrap(~ date) +
    geom_raster() +
    scale_fill_gradientn(colors = cols)
  ggplot(d_small, aes(x, y, fill = last_cut)) +
    coord_equal() +
    facet_wrap(~ date) +
    geom_raster() +
    scale_fill_gradientn(colors = cols)
  ggplot(d_small, aes(x, y, fill = prop_cut)) +
    coord_equal() +
    facet_wrap(~ date) +
    geom_raster() +
    scale_fill_gradientn(colors = cols)
}

#' *TO DO* areas should not be burned if clear-cut afterwards (and vice-versa)
d_lab <-
  d_unnested %>%
  mutate(last_event =case_when(last_fire == 0 & last_cut == 0 ~ 'no event',
                               last_fire > last_cut ~ 'burned',
                               last_fire < last_cut ~ 'cut'))

# check how many and what kind of successions there are
d_lab %>%
  group_by(x, y) %>%
  summarize(cat = paste(unique(last_event), collapse = ' ')) %>%
  pull(cat) %>%
  unique()
