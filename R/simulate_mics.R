#' simulate_mics
#'
#' function that wraps together all the functions that determine t's distribution,
#' pi and its trends, trends in the mean, component draws, epsilon, covariates,
#' and censors the data
#'
#' @param n
#' @param t_dist t_dist1 = function(n) {runif(n, min = 0, max = 1)}
#' @param pi pi = function(t) {z <- 0.5 + 0.2 * t     c(z, 1- z)}
#' @param complist
#' @param sd_vector
#' @param covariate_list
#' @param covariate_effect_vector
#' @param low_con
#' @param high_con
#' @param tested_concentrations
#'
#' @return
#' @export
#'
#' @importFrom dplyr tibble mutate inner_join group_by
#' @importFrom magrittr %>%
#' @importFrom purrr map as_vector
#'
#' @examples
simulate_mics <- function(n = 100,
                          t_dist = function(n){runif(n, min = 0, max = 1)},
                          pi = function(t) {z <- 0.5 + 0.2 * t
                          c("1" = z, "2" = 1- z)},
                          complist = list(
                            "1" = function(t) {3 + t + 2*t^2 -sqrt(t)},
                            "2" = function(t) {3*t}
                          ),
                          sd_vector = c("1" = 1, "2" = 2),
                           covariate_list,
                           covariate_effect_vector,
                           low_con = 2^-4,
                           high_con = 2^4,
                           tested_concentrations = log2(low_con):log2(high_con)){
if(is.null(covariate_list)){
  base_data <- draw_epsilon(n, t_dist, pi, complist, sd_vector)
  simulated_obs <- base_data %>% mutate(observed_value = epsilon + x)
  censored_obs <- censor_values(simulated_obs$observed_value, low_con, high_con, tested_concentrations)
  inner_join(simulated_obs, censored_obs, by = "observed_value")
  } else{
  base_data <- draw_epsilon(n, t_dist, pi, complist, sd_vector)
  covariate_data <- add_covariate(covariate_list = covariate_list, input = base_data$t)
  merged_data <- tibble(base_data, covariate_data)
  total_cov_effect <- covariate_effect_total(merged_data, covariate_effect_vector)
  simulated_obs <- tibble(merged_data, total_cov_effect) %>%
    mutate(observed_value = epsilon + total_cov_effect + x)
  censored_obs <- censor_values(simulated_obs$observed_value, low_con, high_con, tested_concentrations)
  inner_join(simulated_obs, censored_obs)
  }
}


