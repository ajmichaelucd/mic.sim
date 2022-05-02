covariate_list_2 <- list(
  list(c("numeric", "normal", 40, 4), c(0.01)),
  list(c("categorical", 0.3, 0.4, 0.3), c(0.1, 0.2)),
  list(c("categorical", 0.5, 0.2, 0.3), c(0.1, 0.2)),
  list(c("categorical", 0.2, 0.2, 0.2, 0.2, 0.2), c(0.02, 0.03, 0.04, 0.05)),
  list(c("numeric", "uniform", 1, 10), c(0.04))
)

#cov_list
#input corresponds to the

find_covariate_effects <- function(input, cov_list){
  covariate_entries <- input %>%
    select(-(1:2))

  purrr::map2(covariate_entries, cov_list, ~calculate_single_covariate_effect(.x, .y))


}


calculate_single_covariate_effect <-
  function(covariate_entry_column,
           covariate_list_of_lists) {
    if (covariate_list_of_lists[[1]][1] == "numeric") {
      tibble(change = covariate_entry_column * covariate_list_of_lists[[2]])
    }
    else if (covariate_list_of_lists[[1]][1] == "categorical") {
      a <- tibble(levels = covariate_entry_column)
      b <- tibble(levels = c(letters[1:(length(covariate_list_of_lists[[2]]) + 1)]), change = c(0, covariate_list_of_lists[[2]]))
      left_join(a, b, by = "levels") %>%
        select(2)
    }
    else {print("typo")}
    }

calculate_single_covariate_effect(simulated_data$covariate_2, covariate_list_of_lists = covariate_list_2[[2]])



find_covariate_effects(simulated_data, covariate_list_2)




##now add cov effects to base:
sim_data_and_add_covariates <- function(nyears = 5, sample_size_dist = "normal", norm_mean = 100, norm_sd = 10, unif_min, unif_max, covariate_list, covariate_list_2,
                                        initial_lower_mean = -1, initial_upper_mean = 1, initial_lower_std_dev = 1, initial_upper_std_dev = 1, initial_lower_slp = 0, initial_upper_slp = 0, initial_lower_slp_qd = 0, initial_upper_slp_qd = 0,
                                        initial_lower_lambda = 0.5, initial_upper_lambda = 0.5, initial_lower_slp_lambda = 0, initial_upper_slp_lambda = 0){

  drawn_observations <- sim_data(nyears, sample_size_dist, norm_mean, norm_sd, unif_min, unif_max, initial_lower_mean, initial_upper_mean, initial_lower_std_dev, initial_upper_std_dev, initial_lower_slp, initial_upper_slp, initial_lower_slp_qd, initial_upper_slp_qd,
                                 initial_lower_lambda, initial_upper_lambda, initial_lower_slp_lambda, initial_upper_slp_lambda) %>%
    ungroup()

  drawn_covariates <- as_tibble(purrr::map(covariate_list, ~draw_covariates(drawn_observations, .x)),  .name_repair = "unique" )

  names(drawn_covariates) <- create_cov_names(drawn_covariates)

  base_sim <- tibble(drawn_observations, drawn_covariates)

    covariate_effects <- dplyr::as_tibble(find_covariate_effects(base_sim, covariate_list_2)) %>%
      rowwise() %>%
      mutate(covariate_sum = sum(c_across(starts_with("cov"))), .keep = "used") %>%
      ungroup() %>%
      select(covariate_sum)

    tibble(base_sim, covariate_effects) %>%
      mutate(observed_value = base_value + covariate_sum)





  ##Implement covariate effects here



}

bbb %>%
  rowwise() %>%
  mutate(covariate_sum = sum(c_across(starts_with("cov"))), .keep = "used") %>%
  ungroup() %>%
  select(covariate_sum)












