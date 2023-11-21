library(ggplot2)
source('functions/betals-variance-sims-and-derivatives.R')
source('analysis/ggplot_theme.R')
#import data and most recent model
d <- readRDS('data/labelled-ndvi-data.rds')
m <- readRDS('models/betals-gamls-2023-11-08.rds')

newd_doy<- expand_grid(doy = 0:365,
                        year = 0,
                        x_alb = 0,
                        y_alb = 0,
                        event = c('0', 'f', 'c'),
                        years_since=0:103)

preds_doy <- 
  bind_cols(newd_doy,
            predict(m, newdata = newd_doy,
                    terms = c('s(doy)', 's(doy,event)',
                              's(sqrt(years_since),event)', 's.1(doy)', 's.1(doy,event)', 
                              's.1(sqrt(years_since),event)')))
                    
mean_doy<-betals_mean(m, preds_doy, nsims=1e4)%>%
  group_by(doy,event)%>%
  summarize(mu = median(mean),
            lwr.mu = quantile(mean, probs = 0.025),
            upr.mu = quantile(mean, probs = 0.92))#avg between 89 and 95


variance_year <- betals_var(m,preds_doy, nsims = 1e4) %>%
  group_by(doy, event) %>%
  summarize(variance_year = median(variance),
            lwr.s2 = quantile(variance, probs = 0.025),
            upr.s2 = quantile(variance, probs = 0.92))
