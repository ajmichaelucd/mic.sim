#SCRIPT TO RUN A NULL SIMULATION: NO T IN THE MODELS FITTED, NO TRENDS IN THE BASE DATA, ONE COMPONENT IN THE BASE DATA
library(mic.sim)
##Covariate inputs--------------------
covariate_effect_vector <- c(0) #0 at start is intercept, then add in the desired coefficients for the covariates

covariate_list <- NULL
covariate_names <- NULL

n=1000

pi1 = function(t) {z <- 1 + 0* t
c("1" = z)}

complist1 = list(
  "1" = function(t) {0 + 0.5 * t})

t_dist1 = function(n){runif(n, min = 0, max = 3)}

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

plot(data.sim$t, data.sim$observed_value)
abline(a = 0, b = 0.5, col = "red")

purrr::map(
  type_list,
  ~ fit_aft(
    data.sim,
    time = "t",
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

purrr::map(c(1:1000), ~run_null_aft_model(covariate_effect_vector = covariate_effect_vector, covariate_list = covariate_list, covariate_names = covariate_names, n = 100, t_dist = t_dist1, pi = pi1, complist = complist1, sd_vector = sd_vector, low_con = 2^-4, high_con = 2^4, type_list = type_list, time = "t", MIC_breakpoint = 0, summary = FALSE, iteration = .x)) %>%
  dplyr::bind_rows() -> bbb




