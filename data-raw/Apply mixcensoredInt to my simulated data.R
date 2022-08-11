library(magrittr)
library(dplyr)

#data generation------------
n=2000

pi1 = function(t) {z <- 0.8
c("1" = z, "2" = 1- z)}

`E[X|T,C]` = function(t, c)
{
  case_when(
    c == "1" ~ 0 + 0 * t,
    c == "2" ~ 3 + 0 * t,
    TRUE ~ NaN
  )
}

t_dist1 = function(n){runif(n, min = 0, max = 1)}

sd_vector = c("1" = 1, "2" = 1)

low_con = 2^-4
high_con = 2^4


data.sim <- simulate_mics(
  n = n,
  t_dist = t_dist1,
  pi = pi1,
  `E[X|T,C]` = `E[X|T,C]`,
  sd_vector = sd_vector,
  covariate_list = covariate_list,
  covariate_effect_vector = covariate_effect_vector,
  low_con = low_con,
  high_con = high_con)


hist(data.sim$observed_value)









mixcensoredInt(y1 = data.sim$observed_value,
               y2 = data.sim$observed_value,
               d = rep(1, nrow(data.sim)),
               wt=rep(1, length(data.sim$observed_value)),
               dist="gaussian",
               n = 2,
               cluster=NULL,
               classify="EM",
               maxiter=10000, tol=1e-6)
#y1 is the right/left censored value, the exact lifetime observation, or for
# interval censoring the lower value of the censoring interval
#y2 is the upper value of the censoring interval
#d is the censoring indicator (0=right censored, 1=event at time,
# 2=left censored, 3=interval censored)
#wt are the weights for the observations
#dist: either the "weibull", "lognormal", or "gaussian" distribution
#n is the number of components
#cluster: start with random initialization of posterior probabilities (=NULL), or
# a matrix with n columns of initial posterior probabilities for the observations
#classify: "EM", "CEM", or "SEM" strategy
#maxiter is the maximum number of iterations
#tol is the convergence criterion








