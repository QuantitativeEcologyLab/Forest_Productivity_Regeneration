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
    pattern = '.tif', full.names = TRUE),
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
  mutate(ndvi = map(ndvi, \(dat) {
    as.data.frame(dat, xy = TRUE, na.rm = FALSE) %>%
      pivot_longer(! c(x, y), values_to = 'ndvi') %>%
      select(-name) # date column already exists
  }),
  last_fire = map(last_fire, \(.r) raster_to_df(.r, 'last_fire')),
  prop_fire = map(prop_fire, \(.r) raster_to_df(.r, 'prop_fire')),
  last_cut = map(last_cut, \(.r) raster_to_df(.r, 'last_cut')),
  prop_cut = map(prop_cut, \(.r) raster_to_df(.r, 'prop_cut'))) %>%
  unnest(ndvi:prop_cut) %>%
  filter(! is.na(ndvi)) %>%
  rename(x_alb = x, y_alb = y)

if(FALSE) {
  d_small <- filter(d_unnested, date %in% unique(date)[1:2]) 
  pal <- rev(terrain.colors(10))
  
  ggplot(d_small, aes(x_alb, y_alb, fill = ndvi)) +
    coord_equal() +
    facet_wrap(~ date) +
    geom_raster() +
    scale_fill_gradientn(colors = pal)
  ggplot(d_small, aes(x_alb, y_alb, fill = last_fire)) +
    coord_equal() +
    facet_wrap(~ date) +
    geom_raster() +
    scale_fill_gradientn(colors = pal)
  ggplot(d_small, aes(x_alb, y_alb, fill = prop_fire)) +
    coord_equal() +
    facet_wrap(~ date) +
    geom_raster() +
    scale_fill_gradientn(colors = pal)
  ggplot(d_small, aes(x_alb, y_alb, fill = last_cut)) +
    coord_equal() +
    facet_wrap(~ date) +
    geom_raster() +
    scale_fill_gradientn(colors = pal)
  ggplot(d_small, aes(x_alb, y_alb, fill = prop_cut)) +
    coord_equal() +
    facet_wrap(~ date) +
    geom_raster() +
    scale_fill_gradientn(colors = pal)
}

# add labels of the last event and the time since
# minimizing dataset size using single-letter factors and integers
d_lab <-
  d_unnested %>%
  mutate(
    # add labels for what the last event was
    event =
      case_when(
        # missing dates for cut and/or fire
        is.na(last_fire) & is.na(last_cut) ~ '0', # no event = control
        is.na(last_fire) & ! is.na(last_cut) ~ 'c', # clear cut
        ! is.na(last_fire) & is.na(last_cut) ~ 'f', # fire
        # dates for both are present
        last_fire > last_cut ~ 'f',
        last_fire < last_cut ~ 'c'),
    # only keep labels if most of the pixel was affected
    event = case_when(event == 'f' & prop_fire >= 0.5 ~ 'f',
                      event == 'c' & prop_cut >= 0.5 ~ 'c',
                      TRUE ~ '0'),
    # calculate number of years since the last event
    #' *assumes the burn or cut occurred within a day*
    dec_date = decimal_date(date),
    years_since_fire = if_else(event == 'f', dec_date - last_fire, 0),
    years_since_cut = if_else(event == 'c', dec_date - last_cut, 0),
    # if a fire/cut happened last, change time since cut/fire to -1
    years_since_fire = if_else(event != 'f', 0, years_since_fire),
    years_since_cut = if_else(event != 'c', 0, years_since_cut),
    # if the last event was a burn/cut, change prop_cut/fire = 0
    prop_fire = if_else(event != 'f', 0, prop_fire),
    prop_cut = if_else(event != 'c', 0, prop_cut),
    # integer year and day of year as integer
    year = as.integer(year(date)),
    doy = as.integer(yday(date)),
    # convert event class to a factor for regression
    event = factor(event, levels = c('c', 'f', '0'))) %>%
  relocate(c(year, doy, ndvi, event, years_since_fire, last_fire,
             prop_fire, years_since_cut, last_cut, prop_cut),
           .after = date) %>%
  select(! dec_date)

# test the labelling code
if(FALSE) {
  # check how many and what kind of successions there are
  # only time two events occur in the same pixel is when cut follows fire 
  d_lab %>%
    group_by(x_alb, y_alb) %>%
    summarize(cat = paste(unique(event), collapse = ' -> ')) %>%
    pull(cat) %>%
    unique() %>%
    cat(sep = '\n')
  
  # data looks ok following a second fire
  sliver <- filter(d_lab, last_fire > 2015) %>%
    slice(1)
  .x_alb <- sliver$x_alb
  .y_alb <- sliver$y_alb
  d_lab %>%
    filter(x_alb == .x_alb, y_alb == .y_alb) %>%
    plot(years_since_fire ~ date, .)
  
  # data looks ok following a second clear-cut
  sliver <- filter(d_lab, last_cut > 2015) %>%
    slice(200)
  .x_alb <- sliver$x_alb
  .y_alb <- sliver$y_alb
  d_lab %>%
    filter(x_alb == .x_alb, y_alb == .y_alb) %>%
    plot(years_since_cut ~ date, .)
  
  # years since fire is set to -1 after the clear-cut
  sliver <- filter(d_lab, last_cut > 2015) %>%
    slice(100)
  .x_alb <- sliver$x_alb
  .y_alb <- sliver$y_alb
  d_lab %>%
    filter(x_alb == .x_alb, y_alb == .y_alb) %>%
    plot(years_since_fire ~ date, .)
  
  rm(sliver, .x_alb, .y_alb) # remove unnecessary objects
  
  # values switch correctly when a burned area changes to cut
  d_lab %>%
    filter(x_alb == .x_alb, y_alb == .y_alb) %>%
    filter(date > '2014-12-01') %>%
    View()
  
  # values change correctly when a new fire occurs
  # we know this because some diffs are negative, which imply the time
  # since the last fire decreased at some point
  filter(d_lab, event == 'fire') %>%
    group_by(x_alb, y_alb) %>% # group by pixel
    arrange(date) %>% # just to be sure
    transmute(diff = c(NA, diff(years_since_fire))) %>% # find adj changes
    pull(diff) %>%
    hist()
  
  # most values are from (positive) 16-day intervals -- the NDVI frequency
  Mode <- function(v) { # to calculate the mode
    ux <- unique(v)
    ux[which.max(tabulate(match(v, ux)))]
  }
  
  filter(d_lab, event == 'fire') %>%
    group_by(x_alb, y_alb) %>% # group by pixel
    arrange(date) %>% # just to be sure
    transmute(diff = c(NA, diff(years_since_fire))) %>% # find adj changes
    pull(diff) %>%
    Mode() # 0.04383562 * 365 = 16 days
  
  rm(Mode)
}

saveRDS(d_lab, 'data/labelled-ndvi-data.rds')
