#Credible intervals for betals-gamls
library('dplyr')   # for data wrangling
library('tidyr')   # for data wrangling
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

#create a new dataset for the function (decrease computation) and filters appropriately
newd_year<-expand_grid(date=0,#expand_grid returns a tibble whereas expand.grid returns a df
                       year= seq(from = 2000, to= 2023, by = 0.01),
                       doy= 0,
                       ndvi=0,
                       event = unique(d$event),
                       years_since=0,
                       x_alb=0,
                       y_alb=0)


mean_year<-betals_mean(m, newd_year, nsims=1e4, unconditional  = FALSE, terms= c('s(year)', 's(year,event)', 's.1(year)', 's.1(year,event)'))%>%
  group_by(year, event)%>%
  summarize(mu = median(mean),
            lwr.mu = quantile(mean, probs = 0.025),
            upr.mu = quantile(mean, probs = 0.975))%>%
  mutate(across(c(mu, upr.mu, lwr.mu), function(x)x*2-1))#back transforming the NDVI

 

variance_year <- betals_var(m,newd_year, nsims = 1e4, unconditional  = FALSE, terms = c('s(year)', 's(year,event)','s.1(year)', 's.1(year,event)')) %>%
  group_by(year, event) %>%
  summarize(variance_year = median(variance),
            lwr.s2 = quantile(variance, probs = 0.025),
            upr.s2 = quantile(variance, probs = 0.975))%>%
  mutate(across(c(variance_year, upr.s2, lwr.s2), function(y)y * 4))#back transforming the NDVI

pal <- c("#000000", "#EE6677", "#228833")#creating a color palette

mean_yearplot<-ggplot(mean_year)+
  #default+
  geom_ribbon(aes(year, ymin = lwr.mu, ymax = upr.mu, fill = event), alpha = 0.3)+
  geom_line(aes(year, mu, color = event), mean_year, linewidth = 1)+
 # ylim(-0.35,1)+
  labs(x = 'Year', y = 'Mean NDVI (\U03BC)') +
  scale_fill_manual(values = pal, name = "Event", labels= c('Cut', 'Burned', 'Control'), aesthetics = c('color', 'fill'))+
  scale_color_manual(values = pal, name = "Event", labels= c('Cut', 'Burned', 'Control'), aesthetics = c('color', 'fill'))+
  theme(legend.position="none")
                     

plot(mean_yearplot)
variance_yearplot<-ggplot(variance_year)+
  #default+
  geom_ribbon(aes(year, ymin = lwr.s2, ymax = upr.s2,fill = event), alpha = 0.3)+
  geom_line(aes(year, variance_year, color = event), variance_year, linewidth = 1)+
  #ylim(-0.35,1)+
  labs(x = 'Year', y = 'Variance in NDVI (\U03C3\u00B2)') +
  scale_fill_manual(values = pal, name = "Event", labels= c('Cut', 'Burned', 'Control'), aesthetics = c('color', 'fill'))+
  scale_color_manual(values = pal, name = "Event", labels= c('Cut', 'Burned', 'Control'), aesthetics = c('color', 'fill'))+
  theme(legend.position="none")
 

plot(variance_yearplot)

plot_grid(
  get_legend(mean_yearplot + theme(legend.position = 'top')),
  plot_grid(mean_yearplot, variance_yearplot,
           labels = c('a.', 'b.')),
  ncol = 1, rel_heights = c(0.1, 1))

