

covariate_list <- list(
  c("numeric", "normal", 40, 4),
  c("categorical", 0.3, 0.4, 0.3),
  c("categorical", 0.5, 0.2, 0.3),
  c("categorical", 0.2, 0.2, 0.2, 0.2, 0.2),
  c("numeric", "uniform", 1, 10)
)


simulated_data <- sim_data_and_add_covariates(covariate_list = covariate_list)


purrr::map(type_list, ~fit_aft(simulated_data$observed_value, simulated_data, .x, summary = "tidy"))



fit_lr(simulated_data, 4)

fit_spaft(simulated_data$observed_value, simulated_data)

