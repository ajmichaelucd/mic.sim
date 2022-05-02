##Matrix Form Approach

##Inputs
covariate_effect_vector <- c(0, #0 at start is intercept
                             0.01,
                             c(0.05, 0.08),
                             c(0.2, 0.3),
                             c(0.02, 0.04, 0.06, 0.08),
                             0.05
)

covariate_list <- list(
  c("numeric", "normal", 40, 4),
  c("categorical", 0.3, 0.4, 0.3),
  c("categorical", 0.5, 0.2, 0.3),
  c("categorical", 0.2, 0.2, 0.2, 0.2, 0.2),
  c("numeric", "uniform", 1, 10)
)


year <- create_year()


##Operations
data.sim <- simulate_mics(year = year, mean_2_trend = 0.2 ,covariate_list = covariate_list, covariate_effect_vector = covariate_effect_vector)

purrr::map(type_list, ~fit_aft(data.sim$observed_value, data.sim, .x, summary = TRUE))


