library(ggplot2)
library(dplyr)
library('ggplot2') # for fancy plots
library('mgcv')    # for GAMs
library('gratia')  # for visualizing GAMs
library('tidyr')
library('cowplot')
source('functions/betals-variance-sims-and-derivatives.R')
source('analysis/ggplot_theme.R')
#import data and most recent model
d <- readRDS('data/labelled-ndvi-data.rds')
m <- readRDS('models/betals-gamls-2024-03-19.rds')

newd_doy<-expand_grid(date=0,#expand_grid returns a tibble whereas expand.grid returns a df
                                 year= 0,
                                 doy= seq(from = 0, to= 365, by = 0.01),
                                 ndvi=0,
                                 event = unique(d$event),
                                 years_since=0,
                                 x_alb=0,
                                 y_alb=0)
                    
mean_doy<-betals_mean(m, newd_doy, nsims=1e4, unconditional = FALSE, terms = c('s(doy)', 's(doy,event)', 's.1(doy)', 's.1(doy,event'))%>%
  group_by(doy,event)%>%
  summarize(mu = median(mean),
            lwr.mu = quantile(mean, probs = 0.025),
            upr.mu = quantile(mean, probs = 0.975))%>%
  mutate(across(c(mu, upr.mu, lwr.mu), function(x)x*2-1))#back transforming the NDVI#back transforming the NDVI across all 3 coloumns 


variance_doy <- betals_var(m,newd_doy, nsims = 1e4, terms = c('s(doy)', 's(doy,event)', 's.1(doy)', 's.1(doy,event')) %>%#change this with new model
  group_by(doy, event) %>%
  summarize(variance_doy = median(variance),
            lwr.s2 = quantile(variance, probs = 0.025),
            upr.s2 = quantile(variance, probs = 0.975))%>%
  mutate(across(c(variance_doy, upr.s2, lwr.s2), function(y)y * 4))#back transforming the NDVI


pal <- c("#000000", "#EE6677", "#228833")#creating a color palette

mean_doyplot<-ggplot(mean_doy)+
  geom_ribbon(aes(doy, ymin = lwr.mu, ymax = upr.mu,
                  fill = event), alpha = 0.2)+
  geom_line(aes(doy, mu, color = event), mean_doy, linewidth = 1)+
  labs(x = 'Day of Year', y = 'Mean NDVI (\U03BC)') +
  scale_x_continuous(expand =c(0,0))+
  #ylim(-0.35,1)+
  scale_fill_manual(values = pal, name = "Event", labels= c('Cut', 'Burned', 'Control'), aesthetics = c('color', 'fill'))+
  scale_color_manual(values = pal, name = "Event", labels= c('Cut', 'Burned', 'Control'), aesthetics = c('color', 'fill'))+
  theme(legend.position="none")
plot(mean_doyplot)

variance_doyplot<-ggplot(variance_doy)+
  geom_ribbon(aes(doy, ymin = lwr.s2, ymax = upr.s2,
                  fill = event), alpha = 0.3)+
  geom_line(aes(doy, variance_doy, color = event), variance_doy, linewidth = 1)+
  labs(x = 'Day of Year', y = 'Variance in NDVI (\U03C3\u00B2)') +
  scale_x_continuous(expand =c(0,0))+
  #ylim(-0.35,1)+
  scale_fill_manual(values = pal, name = "Event", labels= c('Cut', 'Burned', 'Control'), aesthetics = c('color', 'fill'))+
  #values = pal inserts that manual colour scheme made from above
  scale_color_manual(values = pal, name = "Event", labels= c('Cut', 'Burned', 'Control'), aesthetics = c('color', 'fill'))+
  theme(legend.position="none")

plot(variance_doyplot)

doy_plots <-plot_grid(
  get_legend(mean_doyplot + theme(legend.position = 'top')),
  plot_grid(mean_doyplot, variance_doyplot,
            labels = c('a.', 'b.')),
  ncol = 1, rel_heights = c(0.1, 1))
plot(doy_plots)
