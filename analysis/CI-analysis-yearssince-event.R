library(ggplot2)
library(tidyr) # for expand_grid
library(dplyr)
library(mgcv)
library(cowplot)
source('functions/betals-variance-sims-and-derivatives.R')
source('analysis/ggplot_theme.R')


#import data and most recent model
d <- readRDS('data/labelled-ndvi-data.rds')
m <- readRDS('models/betals-gamls-2024-02-21.rds')


newd_sqrt<- expand_grid(date=0,#expand_grid returns a tibble whereas expand.grid returns a df
                                  year= 0,
                                  doy= 0,
                                  ndvi=0,
                                  event = unique(d$event),
                                  years_since=0:107,#could consider cutting this off in the 99th quantile 
                                  x_alb=0,
                                  y_alb=0)



mean_yearssince<-betals_mean(m, newd_sqrt, nsims=1e4, unconditional = FALSE, terms = c('s(sqrt(years_since),event)', '(Intercept)','s.1(sqrt(years_since),event)', '(Intercept).1'))%>% 
  group_by(years_since, event)%>%
  summarize(mu = median(mean),
            lwr.mu = quantile(mean, probs = 0.025),
            upr.mu = quantile(mean, probs = 0.975))%>%
  mutate(across(c(mu, upr.mu, lwr.mu), function(x)x*2-1)) %>% #undoing transformation across all 3 cols
  filter(! (event == '0' & years_since > 0))

variance_yearssince <- betals_var(m, newd_sqrt, nsims = 1e4,terms = c('s(sqrt(years_since),event)', 's.1(sqrt(years_since),event)', '(Intercept)', '(Intercept).1')) %>%
  group_by(years_since,event) %>%
  summarize(s2 = median(variance),
            lwr.s2 = quantile(variance, probs = 0.025),
            upr.s2 = quantile(variance, probs = 0.975))%>%
   mutate(across(c(s2, upr.s2, lwr.s2), function(x)x*4))%>%#undoing transformation across all 3 cols
  filter(! (event == '0' & years_since > 0))

pal <- c("#000000", "#EE6677", "#228833")#creating a color palette


mean_ysplot<-ggplot(mean_yearssince)+
  geom_ribbon(aes(years_since, ymin = lwr.mu, ymax = upr.mu,
                  fill = event), alpha = 0.3)+
  geom_line(aes(years_since, mu, color = event), mean_yearssince, linewidth = 1)+
  geom_hline(aes(yintercept = mu, color = event), filter(mean_yearssince, event == 0)) +
  geom_hline(aes(yintercept = upr.mu, color = event), filter(mean_yearssince, event == 0), lty = 'dashed') +
  geom_hline(aes(yintercept = lwr.mu, color = event), filter(mean_yearssince, event == 0), lty = 'dashed') +
  geom_point(aes(years_since, mu, color = event), filter(mean_yearssince, event == 0)) +
  geom_errorbar(aes(years_since, ymin = lwr.mu, ymax = upr.mu, color = event), filter(mean_yearssince, event == 0),
                width = 1) +
  scale_fill_manual(values = pal, name = "Event", labels= c('Cut', 'Burned', 'Control'), aesthetics = c('color', 'fill'))+
  scale_color_manual(values = pal, name = "Event", labels= c('Cut', 'Burned', 'Control'), aesthetics = c('color', 'fill'))+
  labs(x = 'Years Since an Event', y = 'Mean NDVI (\U03BC)') +
  theme(legend.position="none")

plot(mean_ysplot)


variance_ysplot<-ggplot(variance_yearssince)+
  geom_ribbon(aes(years_since, ymin = lwr.s2, ymax = upr.s2,
                  fill = event), alpha = 0.3)+
  geom_line(aes(years_since, s2, color = event), variance_yearssince, linewidth = 1)+ #adding line for variance
  geom_hline(aes(yintercept = s2, color = event), filter(variance_yearssince, event == 0), linewidth=0.1) + #making the event '0' as that is the control
  geom_hline(aes(yintercept = upr.s2, color = event), filter(variance_yearssince, event == 0), lty = 'dashed') +
  geom_hline(aes(yintercept = lwr.s2, color = event), filter(variance_yearssince, event == 0), lty = 'dashed') +
  geom_point(aes(years_since, s2, color = event), filter(variance_yearssince, event == 0)) +
  geom_errorbar(aes(years_since, ymin = lwr.s2, ymax = upr.s2, color = event), filter(variance_yearssince, event == 0),
                width = 1) +
  scale_fill_manual(values = pal, name = "Event", labels= c('Cut', 'Burned', 'Control'), aesthetics = c('color', 'fill'))+
  scale_color_manual(values = pal, name = "Event", labels= c('Cut', 'Burned', 'Control'), aesthetics = c('color', 'fill'))+
  labs(x = 'Years Since an Event', y = 'Variance in NDVI (\U03C3^2)') + #figure out how ot fix the sigma^2
  coord_cartesian(ylim = c(0, 0.05)) +
  theme(legend.position="none")
plot(variance_ysplot)

ys_plots <-plot_grid(
  get_legend(mean_ysplot + theme(legend.position = 'top')),
  plot_grid(mean_ysplot, variance_ysplot,
            labels = c('a.', 'b.')),
  ncol = 1, rel_heights = c(0.1, 1))
plot(ys_plots)
