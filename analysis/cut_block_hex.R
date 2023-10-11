library(ggplot2)

#creating hex plot for cut block size vs year
cutblocksize<- ggplot(cuts, aes(x=date, y=are_km2))+
  geom_hex()+
  xlab("Year")+
  ylab(bquote(Cut~Block~Size~(km^2)))

plot(cutblocksize)
