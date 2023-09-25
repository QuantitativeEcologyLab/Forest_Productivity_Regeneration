library('dplyr') # for data wrangling
library('mgcv')  # for GAMs

d <- readRDS('data/labelled-ndvi-data.rds') %>%
  slice_sample(n = 1e4) # only use a sample to start

m <- bam(formula = (ndvi + 1) / 2 ~
           # change of ndvi over space
           
           # seasonal change in ndvi
           
           # long-term change in ndvi
           
           # change in seasonal trend over the years
           
           # recovery time post fire; intercepts are included in the smooth
           
           # recovery time post cut; intercepts are included in the smooth
         ,
         family = betar(link = 'logit'), # because data is in [0, 1] range
         data = d,
         method = 'fREML', # fast REML for faster fitting
         discrete = TRUE, # discretized covariates for faster fitting
         control = gam.control(trace = TRUE), # print updates while fitting
         knots = list(doy = c(0, 1))) # force s(doy) to cover 0 and 1

# ensure the model complexity is reasonable
plot(m, pages = 1, scheme = 3, scale = , all.terms = TRUE)

layout(matrix(1:4, ncol = 2))
gam.check(m, type = 'pearson') # person residuals work better for beta fam
layout(1)

summary(m)
