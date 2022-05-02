

covariate_list <- list(
  c("numeric", "normal", 40, 4),
  c("categorical", 0.3, 0.4, 0.3),
  c("categorical", 0.5, 0.2, 0.3),
  c("categorical", 0.2, 0.2, 0.2, 0.2, 0.2),
  c("numeric", "uniform", 1, 10)
)

covariate_list_2 <- list(
  list(c("numeric", "normal", 40, 4), c(0.01)),
  list(c("categorical", 0.3, 0.4, 0.3), c(0.1, 0.2)),
  list(c("categorical", 0.5, 0.2, 0.3), c(0.1, 0.2)),
  list(c("categorical", 0.2, 0.2, 0.2, 0.2, 0.2), c(0.02, 0.03, 0.04, 0.05)),
  list(c("numeric", "uniform", 1, 10), c(0.04))
)

#implement creating covariate_list_2 from covariate list at some point too

simulated_data <- sim_data_and_add_covariates(covariate_list = covariate_list, covariate_list_2 = covariate_list_2)


purrr::map(type_list, ~fit_aft(simulated_data$observed_value, simulated_data, .x, summary = "tidy"))



fit_lr(simulated_data, 4)

fit_spaft(simulated_data$observed_value, simulated_data)




