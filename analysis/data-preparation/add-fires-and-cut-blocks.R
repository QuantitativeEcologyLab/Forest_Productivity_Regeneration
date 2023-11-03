# for each ndvi raster, get all past fires, then create a raster of last fire date
library('dplyr')     # for data wrangling
library('tidyr')     # for data wrangling
library('purrr')     # for functional programming
library('sf')        # for spatial data
library('lubridate') # for working with dates
library('terra')     # for working with rasters
library('ggplot2')   # for fancy plots (checking data at the end)
source('functions/find_prop.R') # to find proportioned burned
source('functions/find_dates.R') # to find date of last fire
theme_set(theme_bw())

bc <- filter(canadianmaps::PROV, PT == 'BC') %>% # map of BC
  st_geometry() %>% # extract geometry only; drop attributes
  st_as_sf() %>% # convert sfc_MULTIPOLYGON to sf object
  st_transform('EPSG:3005') # project to the BC Albers projection

area <- st_read('data/study-area/study-area-albers.shp') # study area shp
fires <- st_read('data/fires/study-fires.shp') # fire shapefile
cuts <- st_read('data/cut-blocks/study-cut-blocks.shp') # cut block shp
lakes <- st_read('data/lakes/lakes-albers.shp')

# create a standard raster
r <- rast('data/ndvi-rasters/study-area-albers/VI_16Days_250m_v61/NDVI/MOD13Q1_NDVI_2000_049.tif') %>%
  `values<-`(1) %>%
  mask(area)
plot(r)

# rasterize the shapefile of lakes
w <- terra::rasterize(lakes, r, cover = TRUE, background = 0) %>%
  mask(area)
plot(w)
hist(values(w)) # most pixels don't have water
hist(values(w)[values(w) > 0]) # no clear cutoff for what has "lots of water"
mean(values(w) > 0, na.rm = TRUE) # only < 2% of pixels have water
land <- ifel(w == 0, 1, NA)
plot(land)
# take product to set lakes to NA
plot(rast('data/ndvi-rasters/study-area-albers/VI_16Days_250m_v61/NDVI/MOD13Q1_NDVI_2000_049.tif') * land)

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
    .r <- .r * land # drop cells in lakes
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

if(FALSE) {
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
  saveRDS(d_unnested, 'data/d_unnested.rds')
} else {
  d_unnested <- readRDS('data/d_unnested.rds')
}

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
  rm(d_small, pal)
}

# add labels of the last event and the time since
# minimizing dataset size using single-letter factors and integers
d_lab <-
  d_unnested %>%
  mutate(
    # add labels for what the last event was
    event =
      case_when(
        # missing dates for cut or fire
        ! is.na(last_fire) & is.na(last_cut) & prop_fire >= 0.5  ~ 'f',
        is.na(last_fire) & ! is.na(last_cut) & prop_cut >= 0.5  ~ 'c',
        # dates for both are present
        last_fire > last_cut & prop_fire >= 0.5 ~ 'f',
        last_fire < last_cut & prop_cut >= 0.5 ~ 'c',
        # cut or fire occurred after, but in less than half of the pixel
        last_fire > last_cut & prop_fire < 0.5 ~ 'c',
        last_fire < last_cut & prop_cut < 0.5 ~ 'f',
        # otherwise there was no last event
        TRUE ~ '0'),
    # calculate number of years since the last event
    #' *assumes the burn or cut occurred within a day*
    dec_date = decimal_date(date),
    years_since_fire = if_else(event == 'f', dec_date - last_fire, 0),
    years_since_cut = if_else(event == 'c', dec_date - last_cut, 0),
    # if the last event was a burn/cut, change prop_cut/fire = 0
    prop_fire = if_else(event != 'f', 0, prop_fire),
    prop_cut = if_else(event != 'c', 0, prop_cut),
    # integer year and day of year as integer
    year = as.integer(year(date)),
    doy = as.integer(yday(date)),
    # make a single column of years since the last event
    years_since = case_when(event == '0' ~ 0,
                            event == 'f' ~ years_since_fire,
                            event == 'c' ~ years_since_cut),
    # convert event class to a factor for regression
    event = factor(event, levels = c('c', 'f', '0'))) %>%
  # only keep necessary columns
  select(c(date, year, doy, ndvi, event, years_since, x_alb, y_alb))

# test the labelling code
if(FALSE) {
  # check how many and what kind of successions there are
  # only time two events occur in the same pixel is when cut follows fire 
  d_lab %>%
    group_by(x_alb, y_alb) %>%
    arrange(date) %>%
    summarize(cat = paste(unique(event), collapse = ' -> ')) %>%
    pull(cat) %>%
    unique() %>%
    cat(sep = '\n')
  
  # data looks ok following a second fire
  sliver <-
    d_lab %>%
    filter(event == 'f', decimal_date(date) - years_since > 2015) %>%
    slice(1)
  .x_alb <- sliver$x_alb
  .y_alb <- sliver$y_alb
  d_lab %>%
    filter(x_alb == .x_alb, y_alb == .y_alb) %>%
    plot(years_since ~ date, .)
  
  # data looks ok following a second clear-cut
  sliver <- 
    d_lab %>%
    filter(event == 'c', decimal_date(date) - years_since > 2015) %>%
    slice(200)
  .x_alb <- sliver$x_alb
  .y_alb <- sliver$y_alb
  d_lab %>%
    filter(x_alb == .x_alb, y_alb == .y_alb) %>%
    ggplot(aes(date, years_since, color = event)) +
    geom_line()
  
  # values switch correctly when a burned area changes to cut
  d_lab %>%
    filter(x_alb == .x_alb, y_alb == .y_alb) %>%
    filter(date > '2014-12-01') %>%
    View()
  
  # values change correctly when a new fire occurs
  # we know this because some diffs are negative, which imply the time
  # since the last fire decreased at some point
  filter(d_lab, event == 'f') %>%
    group_by(x_alb, y_alb) %>% # group by pixel
    arrange(date) %>% # just to be sure
    transmute(diff = c(NA, diff(years_since))) %>% # find adj changes
    pull(diff) %>%
    hist()
  
  # most values are from (positive) 16-day intervals -- the NDVI frequency
  Mode <- function(v) { # to calculate the mode
    ux <- unique(v)
    ux[which.max(tabulate(match(v, ux)))]
  }
  
  filter(d_lab, event == 'f') %>%
    group_by(x_alb, y_alb) %>% # group by pixel
    arrange(date) %>% # just to be sure
    transmute(diff = c(NA, diff(years_since))) %>% # find adj changes
    pull(diff) %>%
    Mode() # 0.04383562 * 365 = 16 days
  
  rm(Mode)
}

# add NDVI scaled between 0 and 1
d_lab <- d_lab %>%
  mutate(ndvi_scaled = (ndvi + 1) / 2) %>%
  relocate(ndvi_scaled, .after = ndvi)

saveRDS(d_lab, 'data/labelled-ndvi-data.rds')
