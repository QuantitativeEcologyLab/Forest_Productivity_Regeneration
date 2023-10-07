#years on x axis and years since on y, years should be grouped into 10. can apply that filter %10==0
#can combine fires and cutblocks
setwd("C:/Users/danidv.stu/OneDrive - UBC/Directed-Study-main")

library('ggridges')
library('ggplot2')
library('dplyr')

#importing data
d <- readRDS('data/labelled-ndvi-data.rds')

#filtering years into increments of 10
d1<- d%>% 
  filter(d$year %%10==0)

d1$year <- as.factor(d1$year)


#coding for a ridgeline plot using the filtered data
ridgelineplot<- 
  ggplot(d1, aes(x=years_since , y=year, fill=year))+
  geom_density_ridges()+
  theme_ridges()+
  theme(legend.position="none")+
  labs("Number of Years Since Wildfire or Logging Event")+
  xlab("Years Since Event")+
  ylab("Year")+
  scale_fill_brewer(palette = 1) 
 
plot(ridgelineplot)

