library('dplyr') # for data wrangling
library('mgcv')  # for GAMs
library('tidyr')
library('ggplot2')
library('cowplot') # for plot_grid()
source('analysis/ggplot_theme.R')
source('C:/Users/danidv.stu/OneDrive - UBC/Directed-Study-main/functions/betals.r')

d <- readRDS('data/labelled-ndvi-data.rds')

m <- readRDS('models/betals-gamls-2023-10-27.rds')

newd_sqrt <- expand_grid(doy = 0,
                        year = 0,
                        x_alb = 0,
                        y_alb = 0,
                        event = c('0', 'f', 'c'),
                        years_since = seq(0, 50, length.out = 400)) #the oldest cutblock area is from 42 years ago, therefore I am scaling this accordingly

#' since we are just working with a betals model, we have a two link functions
#' `summary()` says link functions are identity functions but they're actually
#' both logit
summary(m)

preds_sqrt <- 
  bind_cols(newd_sqrt,
            predict(m, newdata = newd_sqrt,
                    terms = c('s(sqrt(years_since),event)',
                              's.1(sqrt(years_since),event)'),
                    se.fit = TRUE, type = 'response') %>%
              as.data.frame() %>%
              rename(mu = fit.1, # mean parameers
                     phi = fit.2) %>% # scale parameter
              mutate(sigma2 = phi * (1 - mu) * mu, # calculate variance
                     mu = mu * 2 - 1, # rescale to [-1, 1]
                     sigma2 = sigma2 * 4)) # scale variance appropriately

p_mu_ys <-
  ggplot(preds_sqrt) +
  geom_line(aes(years_since, mu, color = event), lwd = 1) +
  # geom_ribbon(aes(years_since, ymin = mu - 0.1, ymax = mu + 0.1,
  #                 fill = event), alpha = 0.3) +
  labs(x = 'Years Since Event', y = expression(Mean~NDVI~(mu))) +
  scale_color_bright(name = 'Event', labels = c('Control', 'Cut', 'Burned')) +
  scale_fill_bright(name = 'Event', labels = c('Control', 'Cut', 'Burned')) +
  theme(legend.position = 'none')
p_s2_ys <-
  ggplot(preds_sqrt) +
  geom_line(aes(years_since, sigma2, color = event), lwd = 1) +
  labs(x = 'Years Since Event', y = expression(Variance~'in'~'NDVI,'~sigma^2)) +
  scale_color_bright(name = 'Event', labels = c('Control', 'Cut', 'Burned')) +
  theme(legend.position = 'none')

plot_grid(
  get_legend(p_mu_ys + theme(legend.position = 'top')),
  plot_grid(p_mu_ys, p_s2_ys,
            labels = c('a.', 'b.')),
  ncol = 1, rel_heights = c(0.1, 1))

ggsave('10yr_doyevent.jpeg', path= 'C:/Users/danidv.stu/OneDrive - UBC/Directed-Study-main/figures' ,width=6 , height= 4, units= "in")
