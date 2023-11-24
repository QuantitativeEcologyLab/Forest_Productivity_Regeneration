#Credible intervals for betals-gamls
library(ggplot2)
source('functions/betals-variance-sims-and-derivatives.R')
source('analysis/ggplot_theme.R')
#import data and most recent model
d <- readRDS('data/labelled-ndvi-data.rds')
m <- readRDS('models/betals-gamls-2023-11-08.rds')

#create a new dataset for the function (decrease computation) and filters appropriately
newd_year<-expand_grid(date=0,#expand_grid returns a tibble whereas expand.grid returns a df
                       year= 2000:2023,
                       doy= 0,
                       ndvi=0,
                       event = unique(d$event),
                       years_since=0,
                       x_alb=0,
                       y_alb=0)

mean_year<-betals_mean(m, newd_year, nsims=1e4, unconditional  = FALSE, terms= c('s(year)', 's(year,event)', 's.1(year)', 's.1(year,event)'))%>%
  group_by(year,event)#%>%
  summarize(mu = median(mean),
            lwr.mu = quantile(mean, probs = 0.025),
            upr.mu = quantile(mean, probs = 0.975))



variance_year <- betals_var(m,newd_year, nsims = 1e4, unconditional  = FALSE, terms = c('s(year)', 's(year,event)','s.1(year)', 's.1(year,event)')) %>%
  group_by(year, event) %>%
  summarize(variance_year = median(variance),
            lwr.s2 = quantile(variance, probs = 0.025),
            upr.s2 = quantile(variance, probs = 0.975))



