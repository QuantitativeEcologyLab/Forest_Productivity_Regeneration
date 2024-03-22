library('dplyr') # for data wrangling
library('mgcv')  # for GAMs
library('ggplot2')
library('tictoc')
source('functions/betals.r')

d <- readRDS('data/labelled-ndvi-data.rds')

cut_off<-ggplot(d, aes(years_since, fill = event)) +
  geom_density(alpha = 0.3) +
  coord_cartesian(ylim = c(0, 0.15)) +
  geom_vline(xintercept = 50, lty = 'dashed')

# assume that anything past 50 years since the event has regenerated
d <- mutate(d,
            event = if_else(years_since > 70, '0', event),
            years_since = if_else(years_since > 70, 0, years_since))

d$event <- as.factor(d$event)

#Subset fire events
fire_events <- d[d$event == "f", ]

#Subset cut events
cut_events <- d[d$event == "c", ]

#Combine fire and cut events into one dataframe
d_subset <- rbind(fire_events, cut_events)

#Subset control events
control_events <- d[d$event == "0", ]

#Extract random control events, but only 20% of them
subset_control_events <- sample_n(control_events, 3e6)

#Add subset control events to the fire and cut dataframe
d1 <- rbind(d_subset, subset_control_events)


tic()
#' cannot use `bam()` with location-scale families (yet...)
m <- gam(formula = list(
  # linear predictor for the mean (mu)
  ndvi_scaled ~
    ## global changes
    # change of ndvi over space
    s(x_alb, y_alb, bs = 'ds', k = 50) +
    # seasonal change in ndvi
    s(doy, bs = 'cc', k = 10) +
    # yearly change in ndvi
    s(year, bs = 'cr', k = 10) + # cr rather of tp because nrow(d) is high 
    # change in seasonal trend over the years
    ti(doy, year, bs = c('cc', 'cr'), k = c(5, 10)) +
    ## treatment-level changes (intercept are included in smooths)
    # trees affect snow melt over doy
    s(doy, event, bs = 'fs', k = 10, xt = list(bs = 'cc')) +
    # trees affect snow melt over years
    s(year, event, bs = 'fs', k = 10, xt = list(bs = 'cr')) +
    # recovery time post event (fire or cut)
    s(sqrt(years_since), event, bs = 'fs', k = 5),
  # linear predictor for the scale (phi)
  ~ # mgcv requires one-sided formula for the scale parameter
    ## global changes
    # change in phi over space
    s(x_alb, y_alb, bs = 'ds', k = 25) +
    # seasonal change in phi
    s(doy, bs = 'cc', k = 10) +
    # yearly change in ndvi
    s(year, bs = 'cr', k = 10) +
    # change in seasonal trend over the years
    ti(doy, year, bs = c('cc', 'cr'), k = c(5, 5)) +
    ## treatment-level changes (intercept are included in smooths)
    # trees affect snow melt over doy
    s(doy, event, bs = 'fs', k = 10, xt = list(bs = 'cc')) +
    # trees affect snow melt over years
    s(year, event, bs = 'fs', k = 10, xt = list(bs = 'cr')) +
    # recovery time post event (fire or cut)
    s(sqrt(years_since), event, bs = 'fs', k = 6)),
  family = betals(), # because data is in [0, 1] range
  data = d1,
  method = 'REML', # REstricted Maximum Likelihood
  control = gam.control(trace = TRUE), # print updates while fitting
  knots = list(doy = c(0.5, 366.5))) # for doy to span the full year
toc()

saveRDS(m, paste0('models/betals-gamls-', Sys.Date(), '.rds'))

# ensure the model complexity is reasonable
plot(m,
     pages = 1, # plot all terms in a single plot
     scheme = 2, # determines the colors and type of 3D plots
     scale = 0) # independent y axes for each term

# ensure the doy terms are cyclical
layout(matrix(1:4, ncol = 2, byrow = TRUE))
for (p in c(2, 5, 9, 12)) {
  plot(m, scheme = 2, scale = 0, select = p, xlim = c(0.5, 366.5),
       ylim = c(-1.6, 1.6))
}; layout(1)

# basic model diagnostics
if(FALSE) {
  layout(matrix(1:4, ncol = 2))
  gam.check(m, type = 'pearson') # person residuals work better for beta fam
  layout(1)
}

summary(m)
