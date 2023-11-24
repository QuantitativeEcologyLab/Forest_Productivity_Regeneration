library(ggplot2)
source('functions/betals-variance-sims-and-derivatives.R')
source('analysis/ggplot_theme.R')


#import data and most recent model
d <- readRDS('data/labelled-ndvi-data.rds')
m <- readRDS('models/betals-gamls-2023-11-08.rds')


newd_sqrt<- expand_grid(date=0,#expand_grid returns a tibble whereas expand.grid returns a df
                                  year= 0,
                                  doy= 0,
                                  ndvi=0,
                                  event = unique(d$event),
                                  years_since=0:107,
                                  x_alb=0,
                                  y_alb=0)



mean_yearssince<-betals_mean(m, newd_sqrt, nsims=1e4, unconditional = FALSE, terms = c('s(sqrt(years_since),event)','s.1(sqrt(years_since),event)'))%>% 
  group_by(sqrt(years_since))%>%
  summarize(mu = median(mean),
            lwr.mu = quantile(mean, probs = 0.025),
            upr.mu = quantile(mean, probs = 0.975))


variance_yearssince <- betals_var(m,preds_sqrt, nsims = 1e4,terms = c('s(sqrt(years_since),event)', 's.1(sqrt(years_since),event)')) %>%
  group_by(sqrt(years_since)) %>%
  summarize(variance_year = median(variance),
            lwr.s2 = quantile(variance, probs = 0.025),
            upr.s2 = quantile(variance, probs = 0.975))

