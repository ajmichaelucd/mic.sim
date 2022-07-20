

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
    tidyr::pivot_wider(values_from = coef, names_from = coef_name) %>%
    mutate(iteration = iteration)
}



rgr <- function(data.sim, time, covariate_names, left_bound, right_bound, type_list, summary){
  aa <- fit_aft(data.sim, time, covariate_names, left_bound, right_bound, type_list, summary)
  tibble(name = type_list, coef = aa$coefficients,coef_name = names(aa$coefficients))
}


rawr <- function(data.sim, time, covariate_names, left_bound, right_bound, type_list, summary){
  aa <- fit_aft(data.sim, time, covariate_names, left_bound, right_bound, type_list, summary)
  tibble(name = type_list, coef_name = rownames(aa$table), estimate = aa$table[,1], stderr = aa$table[,2])
}



rawr(data.sim, time = "t", covariate_names = NULL, left_bound = data.sim$left_bound, right_bound = data.sim$right_bound, type_list = "lognormal", summary = TRUE)



output_example <- fit_aft(data.sim, time = "t", covariate_names = NULL, left_bound = data.sim$left_bound, right_bound = data.sim$right_bound, type = "lognormal", summary = TRUE)

names(output_example$coefficients)






purrr::map(
  type_list,
  ~ grab_aft_output(
    data.sim,
    time = "t",
    covariate_names,
    data.sim$left_bound,
    data.sim$right_bound,
    .x,
    summary = TRUE
  )
) %>% bind_rows() %>% tidyr::pivot_wider(values_from = coef, names_from = coef_name)











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
    tidyr::pivot_wider(names_from = name, values_from = coef) %>% mutate(iteration = iteration)
}
