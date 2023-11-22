#Credible intervals for betals-gamls
library(ggplot2)
source('functions/betals-variance-sims-and-derivatives.R')
source('analysis/ggplot_theme.R')
#import data and most recent model
d <- readRDS('data/labelled-ndvi-data.rds')
m <- readRDS('models/betals-gamls-2023-11-08.rds')

mean_year<-betals_mean(m, d, nsims=1e4)%>%
  group_by(year,event)%>%
  expand_grid(doy = 0,
              year = 2000:2023,
              x_alb = 0,
              y_alb = 0,
              event = c('0', 'f', 'c'),
              years_since=0)%>%
  summarize(mu = median(mean),
            lwr.mu = quantile(mean, probs = 0.025),
            upr.mu = quantile(mean, probs = 0.92))#avg between 89 and 95


variance_year <- betals_var(m,d, nsims = 1e4) %>%
  group_by(year, event) %>%
  expand_grid(doy = 0,
              year = 2000:2023,
              x_alb = 0,
              y_alb = 0,
              event = c('0', 'f', 'c'),
              years_since=0)%>%
  summarize(variance_year = median(variance),
            lwr.s2 = quantile(variance, probs = 0.025),
            upr.s2 = quantile(variance, probs = 0.92))


 g1<- mean_year%>%
   ggplot(aes(x=year, y= mu, color= event))+
   geom_ribbon(aes(y=lwr.mu, fill=event) )+
   geom_ribbon(aes(y=upr.mu, fill = event))
plot(g1)   


