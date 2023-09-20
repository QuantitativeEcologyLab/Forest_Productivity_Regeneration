# see https://rspatialdata.github.io/vegetation.html for example code and more info
#' install `rgeoboundaries` with `remotes::install_github('wmgeolab/rgeoboundaries')`
#' install `MODIStsp` with `remotes::install_github('ropensci/MODIStsp')`
#' `MODIStsp` version >= 2.0.7 from github fixes login issue due to 
library('dplyr')    # for data wrangling
library('sf')       # for spatial features
library('MODIStsp') # for downloading NDVI rasters
library('tidyr')    # for data wrangling
library('terra')    # to import and save rasters
source('earthdata-login-info.R') # import personal login credentials for EarthData

# download NDVI
MODIStsp(gui = FALSE, # do not use the browser GUI, only run in R
         out_folder = 'data/ndvi-rasters', # '<folder>/VI_16Days_250m_v6/NDVI'
         selprod = 'Vegetation Indexes_16Days_250m (M*D13Q1)', # can't specify Terra here
         prod_version = '061', # 2022 raster version
         bandsel = 'NDVI', # Normalized Difference Vegetation Index layer only
         sensor = 'Terra', # only terrestrial values, ignore main bodies of water
         user = .USERNAME, # your Earthdata username (for urs.earthdata.nasa.gov/home)
         password = .PASSWORD, # your Earthdata password
         start_date = '2000.02.18', # first day of data
         end_date = '2023.02.15', # last day with forward processing
         spatmeth = 'file', # use a bounding box for the extent
         spafile = 'study-area/study-area-albers.shp', # spatial file for the study area
         out_projsel = 'User Defined', # use specified projection instead of default
         output_proj = 'EPSG:3005', # download unprojected raster
         resampling = 'bilinear', # method for resampling raster if changing projection
         delete_hdf = TRUE, # delete HDF files after download is complete
         scale_val = TRUE, # convert from integers to floats within [-1, 1]
         out_format = 'GTiff', # output format: 'ENVI' (.hdr) or 'GTiff' (.tif)
         n_retries = 10, # number of times to try again if download fails before aborting
         verbose = TRUE, # print processing messages
         parallel = 10) # use TRUE for automatic number of cores (max 8), or specify

# check rasters ----
area <- read_sf('data/study-area/study-area-albers.shp')
rasters <-
  list.files(
    path = 'data/ndvi-rasters/study-area-albers/VI_16Days_250m_v61/NDVI/',
    pattern = '.tif', full.names = TRUE) %>%
  rast()

if(FALSE) {
  # plot the rasters
  rasters %>%
    mask(area) %>%
    crop(area) %>%
    terra::plot()
}
