library('dplyr')   # for data wrangling
library('tidyr')   # for data wrangling
library('ggplot2') # for fancy plots
library('mgcv')    # for GAMs
library('gratia')  # for visualizing GAMs
library('tidyr')
library('cowplot')
source('analysis/ggplot_theme.R')
#import data and most recent model
d <- readRDS('data/labelled-ndvi-data.rds')


pal <- c("#000000", "#EE6677", "#228833")
#cut_off<-
  ggplot(d, aes(years_since, fill = event)) +
  geom_histogram(alpha = 0.3, bins = 60, position = "identity") +#change to histogram
  #coord_cartesian(ylim= c(0, 3e5)) +
  geom_vline(xintercept = 70, lty = 'dashed')+
  scale_fill_manual(values = pal, name = "Event", labels= c('Cut', 'Burned', 'Control'), aesthetics = c('color', 'fill'))+
  scale_color_manual(values = pal, name = "Event", labels= c('Cut', 'Burned', 'Control'), aesthetics = c('color', 'fill'))+
  xlab("Years Since")+
  ylab("Number of Data Points") +
  #scale_y_log10(limits = c(1, 1e+7), expand = c(0,0)) +
    scale_y_log10(limits = c(1, 10000000), expand = c(0,0),
                  breaks = c(1+1,10+1,100+1,1000+1,10000+1,100000+1, 1000000+1, 10000000+1),
                  labels = sprintf("%d", c(1,10,100,1000,10000, 100000, 1000000, 10000000))) +
    annotation_logticks(sides = "l",
                        size = 0.4,
                        short = unit(0.09, "cm"),
                        mid = unit(0.09, "cm"),
                        long = unit(0.3, "cm"))

plot(cut_off)
