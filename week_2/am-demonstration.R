library(tidyverse)
## 1. Parametric statistical model

# simulate a normally distributed random variable (y) with a linear predictor (x)

# n = 20 (samples)
n = 30
# parameters of linear predictor
intercept = 0
slope = 1

x <- runif(n = n, min = -1, max = 1)
y <- rnorm(n = n, mean = intercept + slope * x, 
           sd = 0.1)

# combine for plotting
dat <- tibble(x = x, y = y)

# visualise
ggplot() + 
  geom_point(data = dat, 
             aes(x = x, y = y)) + 
  # plot known (true) relationship
  geom_abline(intercept = intercept,
              slope = slope) +
  # plot relationship estimated with linear regression
  stat_smooth(data = dat,
              aes(x = x, y = y),
              method = 'lm')

# fit model so as we can inspect estimated parameters
lm_mod <- lm(y ~ x, data = dat)
summary(lm_mod)

# visual inspection of residuals and fit
plot(fitted(lm_mod), resid(lm_mod));abline(h = 0, lty = 2)
plot(dat$x, resid(lm_mod));abline(h = 0, lty = 2)
plot(fitted(lm_mod), dat$y);abline(c(0,1), lty = 2)


## 2. Nonparametric statistical model
## demonstrate regularisation

n = 20
x1 <-  runif(n = n, min = -1, max = 1)
# x2 <- runif(n = n, min = -1, max = 1)
# x3 <- runif(n = n, min = -1, max = 1)
intercept = 0
beta1 <- 1
beta2 <- 1.2
beta3 <- -1.2

# true relationship: 3rd order polynomial
# known functional form
ytrue <- intercept + 
  beta1 * x1 +
  beta2 * x1^2 + 
  beta3 * x1^3

# add some noise (error) to the relationship
y01 <- ytrue + rnorm(n = n, mean = 0, sd = 0.1)
# more noise
y1 <- ytrue + rnorm(n = n, mean = 0, sd = 1)

# combine for plotting
dat2 <- tibble(x1 = x1,
               x2 = x2, 
               x3 = x3,
               ytrue = ytrue,
               y01 = y01,
               y1 = y1)

# visualise
ggplot() + 
  geom_point(data = dat2, 
             aes(x = x1, y = y01, colour = 'sd = 0.1')) +
  geom_point(data = dat2,
             aes(x = x1, y = y1, colour = 'sd = 1')) +
  geom_line(data = dat2, 
             aes(x = x1, y = ytrue)) +
  # estimate known functional form
  stat_smooth(data = dat2,
              aes(x = x1, y = y01, colour = 'sd = 0.1'),
              formula = y ~ poly(x, 3),
              method = 'lm', se = FALSE) +
  stat_smooth(data = dat2,
              aes(x = x1, y = y1, colour = 'sd = 1'),
              formula = y ~ poly(x, 3),
              method = 'lm', se = FALSE) +
  # estimate unknown functional form (with regularisation)
  stat_smooth(data = dat2,
              aes(x = x1, y = y01, colour = 'sd = 0.1'),
              method = 'gam', se = T, lty = 2,
              formula = y ~ s(x, bs = 'cs')) +
  stat_smooth(data = dat2,
              aes(x = x1, y = y1, colour = 'sd = 1'),
              method = 'gam', se = T, lty = 2,
              formula = y ~ s(x, bs = 'cs')) +
  scale_colour_manual(values = c('sd = 0.1' = '#1b9e77',
                                 'sd = 1' = '#7570b3'))

library(mgcv)
