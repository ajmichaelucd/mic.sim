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
  "1" = function(t) {0 + 0.5 * t})

t_dist1 = function(n){runif(n, min = 0, max = 4)}

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
abline(a = 0, b = 1000, col = "blue")
min(data.sim$observed_value)


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

lm1 = lm(Sepal.Length ~ 1, data = iris)
get_se = function(model, parameters = c("(Intercept)"))
{
  if(!inherits(model, "summary")) model = summary(model)
   temp = model %>% coef()
  temp2 = temp[parameters, "Std. Error"]
  return(temp2)
}

get_se(lm1, parameters = "(Intercept)")
#map get_se across list
#write another for t_coef, intercept, Log(scale)


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


run_an_aft_model <- function(covariate_effect_vector, covariate_list, covariate_names, n, t_dist, pi, complist, sd_vector, low_con, high_con, type_list, time, MIC_breakpoint, summary, iteration){
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
    tidyr::pivot_wider(names_from = name, values_from = coef) %>%
    mutate(iteration = iteration)
}
#slope 0.5
complist1 = list(
  "1" = function(t) {0 + 0.5 * t})

purrr::map(c(1:10), ~run_an_aft_model(covariate_effect_vector = covariate_effect_vector, covariate_list = covariate_list, covariate_names = covariate_names, n = 100, t_dist = t_dist1, pi = pi1, complist = complist1, sd_vector = sd_vector, low_con = 2^-4, high_con = 2^4, type_list = type_list, time = "t", MIC_breakpoint = 0, summary = FALSE, iteration = .x)) %>%
  dplyr::bind_rows() -> slope_0.5
#slope 1000
complist1 = list(
  "1" = function(t) {0 + 1000 * t})

purrr::map(c(1:1000), ~run_an_aft_model(covariate_effect_vector = covariate_effect_vector, covariate_list = covariate_list, covariate_names = covariate_names, n = 100, t_dist = t_dist1, pi = pi1, complist = complist1, sd_vector = sd_vector, low_con = 2^-4, high_con = 2^4, type_list = type_list, time = "t", MIC_breakpoint = 0, summary = FALSE, iteration = .x)) %>%
  dplyr::bind_rows() -> slope_1000
#slope 0.001
complist1 = list(
  "1" = function(t) {0 + 0.001 * t})

purrr::map(c(1:1000), ~run_an_aft_model(covariate_effect_vector = covariate_effect_vector, covariate_list = covariate_list, covariate_names = covariate_names, n = 100, t_dist = t_dist1, pi = pi1, complist = complist1, sd_vector = sd_vector, low_con = 2^-4, high_con = 2^4, type_list = type_list, time = "t", MIC_breakpoint = 0, summary = FALSE, iteration = .x)) %>%
  dplyr::bind_rows() -> slope_0.001


#add check n_distinct > 1










bbb
#with 2^data, we need to take log2(exp(coef)) to get the coefficient

#no transformation needed for gaussian and logistic
#intercepts are off for weibull and exponential


ccc <- bbb %>% mutate(
  loglogistic = ((loglogistic) / log(2)),
  lognormal = ((lognormal) / log(2))
  #  weibull = log2(exp(weibull)),
  #  exponential = log2(exp(exponential))
)
ccc %>%
  pivot_longer(loglogistic:logistic) %>%
  group_by(name) %>%
  summarize(mean =  mean(value),
            se = sd(value)) #standard error should be 0.1 b/c 1/sqrt(100) = 0.1

ccc %>%
  pivot_longer(loglogistic:logistic) %>%
  mutate(error = abs(value - 0)) %>%
  group_by(name) %>%
  summarize(
    Bias = mean(error),
    MSE = mean((value - 0)^2)
  )



