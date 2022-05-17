#' simulate_mics
#'
#' wraps together find_epsilon, add_covariate, covariate_effect_total to produce a tibble with all those outputs included
#'
#' @param year
#' @param sd_1
#' @param sd_2
#' @param mean_1_trend
#' @param mean_2_trend
#' @param mean_1_intercept
#' @param mean_2_intercept
#' @param pi_1_trend
#' @param pi_1_intercept
#' @param covariate_list
#' @param covariate_effect_vector
#' @param low_con
#' @param high_con
#' @param tested_concentrations
#'
#' @return
#' @export
#'
#' @importFrom dplyr mutate tibble inner_join
#' @importFrom magrittr %>%
#'
#' @examples
simulate_mics <- function(year, sd_1 = 1, sd_2 = 1, mean_1_trend = 0, mean_2_trend = 0, mean_1_intercept = -1, mean_2_intercept = 1, pi_1_trend = 0, pi_1_intercept = 0.5, covariate_list, covariate_effect_vector, low_con = 2^-4, high_con = 2^4, tested_concentrations = log2(low_con):log2(high_con)){
  base_data <- find_epsilon(year, sd_1, sd_2, mean_1_trend, mean_2_trend, mean_1_intercept, mean_2_intercept, pi_1_trend, pi_1_intercept)
  covariate_data <- add_covariate(covariate_list = covariate_list, year = year)
  merged_data <- tibble(base_data, covariate_data)
  total_cov_effect <- covariate_effect_total(merged_data, covariate_effect_vector)
  simulated_obs <- tibble(merged_data, total_cov_effect) %>%
    mutate(observed_value = epsilon + total_cov_effect)
  censored_obs <- censor_values(simulated_obs$observed_value, low_con, high_con, tested_concentrations)
  inner_join(simulated_obs, censored_obs)
}
