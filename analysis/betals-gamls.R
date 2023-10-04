library('dplyr') # for data wrangling
library('mgcv')  # for GAMs
source('../ndvi-stochasticity/functions/betals.r')

d <- readRDS('data/labelled-ndvi-data.rds')

#' cannot use `bam()` with location-scale families (yet...)
m <- gam(formula = list(
  # linear predictor for the mean (mu)
  ndvi_scaled ~
    ## global changes
    # change of ndvi over space
    s(x_alb, y_alb, bs = 'ds', k = 200) +
    # seasonal change in ndvi
    s(doy, bs = 'cc', k = 10) +
    # yearly change in ndvi
    s(year, bs = 'cr', k = 20) + # cr rather of tp because nrow(d) is high 
    # change in seasonal trend over the years
    ti(doy, year, bs = c('cc', 'cr'), k = c(5, 10)) +
    ## treatment-level changes (intercept are included in smooths)
    # trees affect snow melt over doy
    s(doy, event, bs = 'sz', k = 5, xt = list(bs = 'cc')) +
    # trees affect snow melt over years
    s(year, event, bs = 'sz', k = 10) +
    # recovery time post event (fire or cut)
    s(sqrt(years_since), event, bs = 'fs', k = 5),
  # linear predictor for the scale (phi)
  ~ # mgcv requires one-sided formula for the scale parameter
    ## global changes
    # change in phi over space
    s(x_alb, y_alb, bs = 'ds', k = 50) +
    # seasonal change in phi
    s(doy, bs = 'cc', k = 10) +
    # yearly change in ndvi
    s(year, bs = 'cr', k = 10) +
    # change in seasonal trend over the years
    ti(doy, year, bs = c('cc', 'cr'), k = c(5, 5)) +
    ## treatment-level changes (intercept are included in smooths)
    # trees affect snow melt over doy
    s(doy, event, bs = 'sz', k = 5, xt = list(bs = 'cc')) +
    # trees affect snow melt over years
    s(year, event, bs = 'sz', k = 10) +
    # recovery time post event (fire or cut)
    s(sqrt(years_since), event, bs = 'fs', k = 5)),
  family = betals(), # because data is in [0, 1] range
  data = d,
  method = 'REML', # REstricted Maximum Likelihood
  control = gam.control(trace = TRUE), # print updates while fitting
  knots = list(doy = c(0.5, 366.5))) # for doy to span the full year

# ensure the model complexity is reasonable
plot(m, pages = 1, scheme = 2, scale = 0)

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

saveRDS(m, paste0('models/betals-gamls-', Sys.Date(), '.rds'))
