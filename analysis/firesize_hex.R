library(ggplot2)

#creating hex plot for fire size vs year
firesizeplot<- ggplot(fires, aes(x= date, y= size_km2))+
  geom_hex()+
  xlab("Year")+
  ylab(bquote(Fire~Size~(km^2))) #trying to figure out how to make a superscript in the axis label

plot(firesizeplot)


