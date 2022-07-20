require(tidyverse)
require(mixtools)


draw_base <- function(df, year, year_coef, intercept, sd){
mean_i <- intercept + year_coef * (year)
rnorm(1, mean_i, sd)
}

dat <- tibble(year = c(rep(0, 10000),
                rep(1, 15000),
                rep(2, 20000),
                rep(3, 30000)
                )) %>%
  rowwise() %>%
  mutate(value = draw_base(., year = year, year_coef = 1, intercept = 0, sd = 1)) %>%
  ungroup() %>%
  mutate(ub = ceiling(value),
         lb = floor(value))

(Y <- with(dat, Surv(lb, ub, event = rep(3, nrow(dat)), type = "interval")))
(Z <- with(dat, Surv(2^lb, 2^ub, event = rep(3, nrow(dat)), type = "interval")))
(L <- with(dat, Surv(exp(lb), exp(ub), event = rep(3, nrow(dat)), type = "interval")))

survreg(Y ~ year, data = dat, dist = 'weibull') #takes the log of the times, doesn't work on log-transformed data with negative or 0 in it
survreg(Y ~ year, data = dat, dist = 'lognormal') #takes the log of the times, doesn't work on log-transformed data with negative or 0 in it
survreg(Y ~ year, data = dat, dist = 'exponential') #takes the log of the times, doesn't work on log-transformed data with negative or 0 in it
survreg(Y ~ year, data = dat, dist = 'loglogistic') #takes the log of the times, doesn't work on log-transformed data with negative or 0 in it

survreg(Y ~ year, data = dat, dist = 'gaussian') #works just fine with negative or 0 in times
survreg(Y ~ year, data = dat, dist = 'logistic') #works just fine with negative or 0 in times


#with 2^data, we need to divide by ln2 to get the coefficient
survreg(Z ~ year, data = dat, dist = 'weibull') #takes the log of the times
survreg(Z ~ year, data = dat, dist = 'lognormal') #takes the log of the times  #if I take log2(exp(coef)) for this, it is the same as gaussian on Y
survreg(Z ~ year, data = dat, dist = 'exponential') #takes the log of the times
survreg(Z ~ year, data = dat, dist = 'loglogistic') #takes the log of the times

survreg(Z ~ year, data = dat, dist = 'gaussian') #takes the log of the times
survreg(Z ~ year, data = dat, dist = 'exponential')


survreg(L ~ year, data = dat, dist = 'weibull')
survreg(L ~ year, data = dat, dist = 'lognormal') #the same as gaussian on Y
survreg(L ~ year, data = dat, dist = 'exponential')
survreg(L ~ year, data = dat, dist = 'loglogistic')


##CORRECT TRANSFORMATION IS TO DIVIDE BY LN(2) FOR LOGNORMAL AND LOGLOGISTIC



