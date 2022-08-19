rm(list = ls())
set.seed(2)
library(magrittr)
library(dplyr)
#<<<<<<< HEAD
load_all()


#NEXT STEPS:
##DIFFERENT SIGMAS
##INTERVAL CENSORING
##MORE THAN 2 COMPONENTS
##FUNCTIONS OF TIME AND COVARIATES: MEANS AND PI

#=======
library(mic.sim)
library(ggplot2)
library(LearnBayes)
library(survival)
library(biglm)
##Covariate inputs--------------------
covariate_effect_vector <- c(0 #0 at start is intercept, then add in the desired coefficients for the covariates
)

#a is the baseline level for all categorical covariates so, add terms for the coefficients for other levels of the variable that reflect the difference between that level and a

## y = B0 + B1X1 + B2X2b + B3X2c + B4X3b + B5X3c + B6X4b + B7X4c + B8X4d + B9X4e + B10X5 + \epsilon

covariate_list <-
  NULL


covariate_names <- NULL

#>>>>>>> f8a23b83e1c920f1303df831494854a95491ecc4

#data generation------------
n=2000

ncomp = 2

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

data.sim %>%
  ggplot() +
  geom_histogram(aes(x = observed_value, fill = factor(indicator)), binwidth = 1, boundary = 0, color = "black")


data.sim %>% group_by(comp) %>%
  summarise(
    n = n()) %>%
  mutate(total = n / sum(n))




visible_data <- data.sim %>%
  select(t, left_bound, right_bound, true_comp = comp) %>%
  mutate(obs_id = 1:n(),
         left_bound = log2(left_bound),
         right_bound = log2(right_bound)) %>%
  relocate(obs_id, .before = everything())

likelihood_documentation = fit_model(visible_data)

#we need to calculate P(Y|t,c) and add to data set

plot(x = likelihood_documentation[,1], y = likelihood_documentation[,2], type = "l") ##Log likelihood appears to be decreasing???????
browser()
lm(observed_value ~ comp - 1, data = data.sim)

model
#missing from algorithm (aside from extension) is convergence criterion: likelihood and parameters we are estimating, see how much they change by and if they change by less than some amount you stop

data.sim %>% group_by(comp) %>%
  summarise(
    n = n()) %>%
  mutate(total = n / sum(n))

pi



sigma(data.sim_summary)

.391

.549

##Possible issue with weights?
##Main issue is that sigma starts correct, drops a few times (and the likelihood also drops), then sigma increases and likelihood starts to climb, but sigma does not get back to where it started



glm(y ~ c - 1, weights =  `P(C=c|y,t)`, data = possible_data)
sigma(glm(y ~ c - 1, weights =  `P(C=c|y,t)`, data = possible_data))


lm(y ~ c - 1, weights =  `P(C=c|y,t)`, data = old_possible_data)
sigma(lm(y ~ c - 1, weights =  `P(C=c|y,t)`, data = old_possible_data))


lm(y ~ c - 1, weights =  `P(C=c|y,t)`, data = old_possible_data)
sigma(lm(y ~ c - 1, weights =  `P(C=c|y,t)`, data = old_possible_data))

old_possible_data %>%
  mutate(`P(C=c|y,t)` = ifelse(`P(C=c|y,t)` < 0.00001, 0 , `P(C=c|y,t)`))


sum(old_possible_data[1:2,5, drop = TRUE])

possible_data %>% filter(c == 1) %>% ggplot2::ggplot(aes(x = t, y= y, color = `P(C=c|y,t)`)) + geom_point() + facet_wrap(~true_comp) + ggtitle("2")
old_possible_data %>% filter(c == 1) %>% ggplot2::ggplot(aes(x = t, y= y, color = `P(C=c|y,t)`)) + geom_point() + facet_wrap(~true_comp) + ggtitle("1")



