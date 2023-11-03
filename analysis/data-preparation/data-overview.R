library('dplyr') # for data wrangling
library('sf')    # for spatial data

# import a map of BC (same map that can be downloaded from Stats Canada)
bc <- filter(canadianmaps::PROV, PT == 'BC') %>% # map of BC
  st_geometry() %>% # extract geometry only; drop attributes
  st_as_sf() %>% # convert sfc_MULTIPOLYGON to sf object
  st_transform('EPSG:3005') # project to the BC Albers projection

# import a shapefile of the study area
area <- st_read('data/total_area_polygon/totalarea.shp') %>%
  st_set_crs('+proj=longlat') %>% # specify (unprojected) coordinate system
  st_geometry() %>% # extract geometry only; drop attributes
  st_as_sf() %>% # convert sfc_MULTIPOLYGON to sf object
  st_bbox() %>%
  st_as_sfc() %>%
  st_as_sf() %>%
  st_transform(st_crs(bc)) # project to the BC Albers projection

plot(area)
plot(bc)
plot(area, add = TRUE, col = 'red')

# import the cut block shapefiles
cuts <- st_read('data/FTEN_CUT_BLOCK_POLY_SVW/FTN_C_B_PL_polygon.shp') %>%
  st_intersection(area) %>% # only keep cut blocks in the study area
  # rename and wrangle useful variables; drop the rest
  transmute(date = case_when( # change the date to a readable format
    nchar(BLK_ST_DT) == 8 ~ as.Date(BLK_ST_DT, format = '%Y%m%d'),
    nchar(BLK_ST_DT) == 14 ~ as.Date(BLK_ST_DT, format = '%Y%m%d%H%M%S')),
    cut_block_id = CUT_BLK_ID,
    area_km2 = AREA_SQM / 1e6)
st_crs(cuts) # already in BC Albers projection
head(cuts)

plot(area, col = 'forestgreen', main = 'Cut blocks')
plot(st_geometry(cuts), add = TRUE, col = '#964B0070') # geom plots faster

# import the fire data
#' *NOTE:* the shapefile has x, y, and z (altitude) coordinates, which are
#' not compatible with the Albers projection  
fires <- st_read('data/national-fire-database/NFDB_poly_20210707.shp') %>%
  st_zm(drop = TRUE) %>% # drop altitude geometry
  st_transform(st_crs(bc)) %>% # project to the BC Albers projection
  st_intersection(area) %>%
  # rename useful columns, drop the rest
  transmute(fire_id = FIRE_ID,
            date = REP_DATE, # assuming start date is homogeneous
            size_km2 = SIZE_HA / 100) %>% # convert to square km
  # sort by burn date
  arrange(date)

# ensure the fire polygons are all in the study area
plot(st_geometry(area), col = 'forestgreen', main = 'Fires')
plot(st_geometry(fires), add = TRUE, col = '#FF000080')

# shapefile of lakes
lakes <- st_read('data/waterbodies/FWA_LAKES_POLY/FWLKSPL_polygon.shp') %>%
  st_geometry() %>% # drop attributes, only keep geometry
  st_intersection(area)
plot(st_geometry(area), col = 'forestgreen', main = 'Lakes')
plot(lakes, add = TRUE, col = 'steelblue1')

# save the shapefiles
if(! dir.exists('data/cut-blocks')) dir.create('data/cut-blocks')
st_write(cuts, 'data/cut-blocks/study-cut-blocks.shp')

if(! dir.exists('data/fires')) dir.create('data/fires')
st_write(fires, 'data/fires/study-fires.shp')

if(! dir.exists('data/study-area')) dir.create('data/study-area')
st_write(area, 'data/study-area/study-area-albers.shp')

if(! dir.exists('data/lakes')) dir.create('data/lakes')
st_write(lakes, 'data/lakes/lakes-albers.shp')
