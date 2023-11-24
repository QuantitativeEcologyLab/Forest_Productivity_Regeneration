library(ggplot2)
source('functions/betals-variance-sims-and-derivatives.R')
source('analysis/ggplot_theme.R')
#import data and most recent model
d <- readRDS('data/labelled-ndvi-data.rds')
m <- readRDS('models/betals-gamls-2023-11-08.rds')

newd_doy<-expand_grid(date=0,#expand_grid returns a tibble whereas expand.grid returns a df
                                 year= 0,
                                 doy= 0:365,
                                 ndvi=0,
                                 event = unique(d$event),
                                 years_since=0,
                                 x_alb=0,
                                 y_alb=0)

                    
mean_doy<-betals_mean(m, newd_doy, nsims=1e4, unconditional = FALSE, terms = c('s(doy)', 's(doy,event)', 's.1(doy)', 's.1(doy,event'))%>%
  group_by(doy,event)%>%
  summarize(mu = median(mean),
            lwr.mu = quantile(mean, probs = 0.025),
            upr.mu = quantile(mean, probs = 0.975))


variance_year <- betals_var(m,newd_doy, nsims = 1e4) %>%
  group_by(doy, event) %>%
  summarize(variance_year = median(variance),
            lwr.s2 = quantile(variance, probs = 0.025),
            upr.s2 = quantile(variance, probs = 0.975))
