library('ggplot2')
source('analysis/ggplot_theme.R')


d <- readRDS('data/labelled-ndvi-data.rds')

pal <- c("#000000", "#EE6677", "#228833")#creating a color palette

cut_off_plot<-ggplot(d, aes(years_since, fill = event)) +
  geom_density(alpha = 0.3) +
  coord_cartesian(ylim = c(0, 0.15)) +
  geom_vline(xintercept = 50, lty = 'dashed')+
  scale_color_manual(values = pal, name = "Event", labels= c('Cut', 'Burned', 'Control'), aesthetics = c('color', 'fill'))+
  labs(x = 'Years Since an Event', y = 'Density' )
cut_off_plot
