library(magrittr)
library(dplyr)
library(ggplot2)
set.seed(2)
rm(list = ls())
load_all()
#data generation------------
n=2000

pi1 = function(t) {z <- 0.6
c("1" = z, "2" = 1- z)}

`E[X|T,C]` = function(t, c)
{
  case_when(
    c == "1" ~ 0,
    c == "2" ~ 3,
    TRUE ~ NaN
  )
}

t_dist1 = function(n){runif(n, min = 0, max = 1)}

sd_vector = c("1" = 1, "2" = 1)

low_con = 2^-4
high_con = 2^4

##Covariate inputs--------------------
covariate_effect_vector <- c(0 #0 at start is intercept, then add in the desired coefficients for the covariates
)

#a is the baseline level for all categorical covariates so, add terms for the coefficients for other levels of the variable that reflect the difference between that level and a

## y = B0 + B1X1 + B2X2b + B3X2c + B4X3b + B5X3c + B6X4b + B7X4c + B8X4d + B9X4e + B10X5 + \epsilon

covariate_list <-
  NULL


covariate_names <- NULL



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




data.sim %>%
  ggplot() +
  geom_histogram(aes(x = observed_value, fill = factor(indicator)), binwidth = 1, boundary = 0, color = "black") +
  facet_wrap(~comp)

data.sim %>%
  ggplot() +
  geom_histogram(aes(x = observed_value, fill = factor(indicator)), binwidth = 1, boundary = 0, color = "black")



table(data.sim$indicator == 2)
table(data.sim$left_bound)
table(ifelse(data.sim$indicator == 2, data.sim$right_bound, data.sim$left_bound ))



gmm_int_cen <- mixcensoredInt(y1 = ifelse(data.sim$indicator == 2, data.sim$right_bound, data.sim$left_bound ),  #format would be wrong for this, need a conversion factor if using this notation
               y2 = data.sim$right_bound,
               d = data.sim$indicator,
               wt=rep(1, length(data.sim$observed_value)),
               dist="lognormal",
               n = 2,
               cluster=NULL,
               classify="EM",
               maxiter=10000, tol=1e-6)


aaaa <- mixcensoredInt(y1 = ifelse(data.sim$indicator == 2, log2(data.sim$right_bound), log2(data.sim$left_bound )),  #format would be wrong for this, need a conversion factor if using this notation
               y2 = log2(data.sim$right_bound),
               d = data.sim$indicator,
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
gmm_int_cen$components  ###Divide by log2 to get correct things here
gmm_int_cen$prior
gmm_int_cen$loglik
gmm_int_cen$iterations
gmm_int_cen$standardError


plot(x = gmm_int_cen$likelihood_documentation_gmm[,1], y= gmm_int_cen$likelihood_documentation_gmm[,2], type = "l")
lines(x = likelihood_documentation[,1], y = likelihood_documentation[,2])

aaaa$components  ###Divide by log2 to get correct things here
aaaa$prior
aaaa$loglik
aaaa$iterations
aaaa$standardError



