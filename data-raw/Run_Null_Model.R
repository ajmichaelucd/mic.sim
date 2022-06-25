#SCRIPT TO RUN A NULL SIMULATION: NO T IN THE MODELS FITTED, NO TRENDS IN THE BASE DATA, ONE COMPONENT IN THE BASE DATA
library(mic.sim)
##Covariate inputs--------------------
covariate_effect_vector <- c(0) #0 at start is intercept, then add in the desired coefficients for the covariates

covariate_list <- NULL
covariate_names <- NULL

n=100

pi1 = function(t) {z <- 1 + 0* t
c("1" = z)}

complist1 = list(
  "1" = function(t) {0})

t_dist1 = function(n){runif(n, min = 1, max = 1)}

sd_vector = c("1" = 1)

low_con = 2^-4
high_con = 2^4

type_list = c("loglogistic", "lognormal", "weibull", "gaussian", "exponential", "logistic") #list of types of AFT error distributions to use

MIC_breakpoint = 0

data.sim <- simulate_mics(
  n = n,
  t_dist = t_dist1,
  pi = pi1,
  complist = complist1,
  sd_vector = sd_vector,
  covariate_list = covariate_list,
  covariate_effect_vector = covariate_effect_vector,
  low_con = low_con,
  high_con = high_con)

data.sim

purrr::map(
  type_list,
  ~ fit_aft(
    data.sim,
    time = NULL,
    covariate_names,
    data.sim$left_bound,
    data.sim$right_bound,
    .x,
    summary = TRUE
  )
)

grab_aft_output <- function(data.sim, time, covariate_names, left_bound, right_bound, type_list, summary){
  aa <- fit_aft(data.sim, time, covariate_names, left_bound, right_bound, type_list, summary)
  tibble(name = type_list, coef = aa$coefficients)
}


purrr::map(
  type_list,
  ~ grab_aft_output (
    data.sim,
    time = "t",
    covariate_names,
    data.sim$left_bound,
    data.sim$right_bound,
    .x,
    summary = TRUE
  )
) %>% dplyr::bind_rows()


run_null_aft_model <- function(covariate_effect_vector, covariate_list, covariate_names, n, t_dist, pi, complist, sd_vector, low_con, high_con, type_list, time, MIC_breakpoint, summary, iteration){
  data.sim <- simulate_mics(
    n,
    t_dist,
    pi,
    complist,
    sd_vector,
    covariate_list,
    covariate_effect_vector,
    low_con,
    high_con)

  purrr::map(
    type_list,
    ~ grab_aft_output (
      data.sim,
      time,
      covariate_names,
      data.sim$left_bound,
      data.sim$right_bound,
      .x,
      summary
    )
  ) %>% dplyr::bind_rows() %>%
    tidyr::pivot_wider(names_from = name, values_from = coef) %>% mutate(iteration = iteration)
}

purrr::map(c(1:1000), ~run_null_aft_model(covariate_effect_vector = covariate_effect_vector, covariate_list = covariate_list, covariate_names = covariate_names, n = 100, t_dist = t_dist1, pi = pi1, complist = complist1, sd_vector = sd_vector, low_con = 2^-4, high_con = 2^4, type_list = type_list, time = NULL, MIC_breakpoint = 0, summary = FALSE, iteration = .x)) %>%
  dplyr::bind_rows() -> bbb

bbb
#with 2^data, we need to take log2(exp(coef)) to get the coefficient

#no transformation needed for gaussian and logistic
#intercepts are off for weibull and exponential


ccc <- bbb %>% mutate(
  loglogistic = log2(exp(loglogistic)),
  lognormal = log2(exp(lognormal)),
  weibull = log2(exp(weibull)),
  exponential = log2(exp(exponential))
)
ccc %>%
  pivot_longer(loglogistic:logistic) %>%
  group_by(name) %>%
  summarize(mean =  mean(value),
            sd = sd(value)) #standard error should be 0.1 b/c 1/sqrt(100) = 0.1

ccc %>%
  pivot_longer(loglogistic:logistic) %>%
  mutate(error = abs(value - 0)) %>%
  group_by(name) %>%
  summarize(
    Bias = mean(error),
    MSE = mean((value - 0)^2)
  )



