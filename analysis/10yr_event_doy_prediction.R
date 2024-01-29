library('dplyr') # for data wrangling
library('mgcv')  # for GAMs
library('tidyr')
library('ggplot2')
library('cowplot') # for plot_grid()
source('analysis/ggplot_theme.R')


d <- readRDS('data/labelled-ndvi-data.rds')

m <- readRDS('models/betals-gamls-2023-11-08.rds')

newd_doy <- expand_grid(doy = 1:365,
                         year = 0,
                         x_alb = 0,
                         y_alb = 0,
                         event = c('0', 'f', 'c')) %>%
  mutate(years_since = if_else(event == '0', 0, 10))

#' since we are just working with a betals model, we have a two link functions
#' `summary()` says link functions are identity functions but they're actually
#' both logit
summary(m)
inv_logit <- brms:::inv_logit # take inverse logit function from brms package

preds_doy <- 
  bind_cols(newd_doy,
            predict(m, newdata = newd_doy,
                    terms = c('s(doy)', 's(doy,event)',
                              's(sqrt(years_since),event)', 's.1(doy)', 's.1(doy,event)', 
                              's.1(sqrt(years_since),event)'),
                    se.fit = TRUE, type = 'response') %>%
              as.data.frame() %>%
              rename(mu = fit.1, # mean parameers
                     phi = fit.2) %>% # scale parameter
              mutate(sigma2 = phi * (1 - mu) * mu, # calculate variance
                     mu = mu * 2 - 1, # rescale to [-1, 1]
                     sigma2 = sigma2 * 4)) # scale variance appropriately

p_doy_mu<- 
  ggplot(preds_doy) +
  geom_line(aes(doy, mu, color = event), lwd = 1) +
  # geom_ribbon(aes(years_since, ymin = mu - 0.1, ymax = mu + 0.1,
  #                 fill = event), alpha = 0.3) +
  labs(x = 'Day of Year', y = expression(Mean~NDVI~(mu))) +
  scale_color_bright(name = 'Event', labels = c('Control', 'Cut (10 years after)',
                                                'Burned (10 years after)'))+
  theme(legend.position="none")

p_doy_s2<-
  ggplot(preds_doy) +
  geom_line(aes(doy, sigma2, color = event), lwd = 1) +
  # geom_ribbon(aes(years_since, ymin = mu - 0.1, ymax = mu + 0.1,
  #                 fill = event), alpha = 0.3) +
  labs(x = 'Day of Year', y = expression(Variance~'in'~'NDVI,'~sigma^2)) +
  scale_color_bright(name = 'Event', labels = c('Control', 'Cut (10 years after)',
                                                'Burned (10 years after)'))+
  theme(legend.position="none")

plot_grid(
  get_legend(p_doy_mu + theme(legend.position = 'top')),
  plot_grid(p_doy_mu, p_doy_s2,
            labels = c('a.', 'b.')),
  ncol = 1, rel_heights = c(0.1, 1))
