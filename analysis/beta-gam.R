library('dplyr') # for data wrangling
library('mgcv')  # for GAMs

d <- readRDS('data/labelled-ndvi-data.rds')

m <- bam(formula = (ndvi + 1) / 2 ~
           ## global changes
           # change of ndvi over space
           s(x_alb, y_alb, bs = 'ds', k = 200) +
           # seasonal change in ndvi
           s(doy, bs = 'tp', k = 10) +
           # yearly change in ndvi
           s(year, bs = 'tp', k = 20) +
           # change in seasonal trend over the years
           ti(doy, year, bs = c('cc', 'cr'), k = c(5, 10)) +
           ## treatment-level changes (intercept are included in smooths)
           # trees affect snow melt over doy
           s(doy, event, bs = 'sz', k = 5) +
           # trees affect snow melt over years
           s(year, event, bs = 'sz', k = 10) +
           # recovery time post event (fire or cut)
           s(sqrt(years_since), event, bs = 'fs', k = 5),
         family = betar(link = 'logit'), # because data is in [0, 1] range
         data = d,
         method = 'fREML', # fast REML for faster fitting
         discrete = TRUE, # discretized covariates for faster fitting
         control = gam.control(trace = TRUE), # print updates while fitting
         knots = list(doy = c(0.5, 366.5))) # for s(doy) to span full year

# ensure the model complexity is reasonable
plot(m, pages = 1, scheme = 3, all.terms = TRUE)

layout(matrix(1:4, ncol = 2))
gam.check(m, type = 'pearson') # person residuals work better for beta fam
layout(1)

summary(m)
