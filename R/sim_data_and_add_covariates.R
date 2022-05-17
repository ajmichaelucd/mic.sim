
#' Title
#'
#' @param nyears
#' @param sample_size_dist
#' @param norm_mean
#' @param norm_sd
#' @param unif_min
#' @param unif_max
#' @param covariate_list
#' @param covariate_list_2
#' @param initial_lower_mean
#' @param initial_upper_mean
#' @param initial_lower_std_dev
#' @param initial_upper_std_dev
#' @param initial_lower_slp
#' @param initial_upper_slp
#' @param initial_lower_slp_qd
#' @param initial_upper_slp_qd
#' @param initial_lower_lambda
#' @param initial_upper_lambda
#' @param initial_lower_slp_lambda
#' @param initial_upper_slp_lambda
#'
#' @return
#' @export
#'
#' @importFrom purrr map as_vector
#' @importFrom dplyr as_tibble ungroup mutate rowwise tibble c_across
#' @importFrom magrittr %>%
#'
#' @examples
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



