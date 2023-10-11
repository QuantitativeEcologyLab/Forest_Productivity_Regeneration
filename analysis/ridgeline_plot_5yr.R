setwd("C:/Users/danidv.stu/OneDrive - UBC/Directed-Study-main")

library('ggridges')
library('ggplot2')
library('dplyr')

#importing data
d <- readRDS('data/labelled-ndvi-data.rds')

#filtering years into increments of 10
d1<- d%>% 
  filter(d$year %%5==0)

d1$year <- as.factor(d1$year)


#coding for a ridgeline plot using the filtered data
ridgelineplot<- 
  ggplot(d1, aes(x=years_since , y=year))+
  geom_density_ridges()+
  theme(legend.position="none")+
  labs("Number of Years Since Wildfire or Logging Event")+
  xlab("Years Since Event")+
  ylab("Year")+
  xlim(0,10)
#i just need to make these all the same colour
plot(ridgelineplot)


