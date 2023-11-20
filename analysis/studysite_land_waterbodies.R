#making a map of the study site with the cutblock, fire and water-body polygons
library('ggplot2')
library('dplyr') # for data wrangling
library('sf')    # for spatial data
library("terra")
library('cowplot')
# import a map of BC (same map that can be downloaded from Stats Canada)
bc <- filter(canadianmaps::PROV, PT == 'BC') %>% # map of BC
  st_geometry() %>% # extract geometry only; drop attributes
  st_as_sf() %>% # convert sfc_MULTIPOLYGON to sf object
  st_transform('EPSG:3005') # project to the BC Albers projection


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

waterbodies<- st_read('data/waterbodies/FWA_LAKES_POLY/FWLKSPL_polygon.shp')%>%
  st_as_sfc() %>%
  st_as_sf() %>%
  st_transform(st_crs(bc))%>% # project to the BC Albers projection
  st_intersection(area)


plot(area, col= 'yellowgreen') #might alter this colour
plot(waterbodies, add=TRUE, col='blue') #dont add cutblocks or burned areas

ss<-rbind(area,waterbodies)

plot(ss)

largescale <- rbind(bc,area)

main = ggplot()+
  geom_sf(data= area, fill='yellowgreen', colour= 'yellowgreen')+
  geom_sf(data=waterbodies,fill='blue', colour="blue")+
  theme_void()+
  theme(legend.position="none")
plot(main)

inset = ggplot()+
  geom_sf(data= bc, fill= 'white')+
  geom_sf(data = area, fill = 'red')+
  theme_void()

plot(inset)

gg_inset_map= ggdraw()+
  draw_plot(main)+
  draw_plot(inset, x= 0.05, y=0.65, width = 0.3, height =  0.3)
plot(gg_inset_map)

