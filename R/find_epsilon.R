#' find_epsilon
#'
#' finds the base value for each observation by drawing from a gaussian mixture with 2 components
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
#'
#' @return
#' @export
#'
#' @importFrom dplyr tibble rowwise mutate ungroup
#' @importFrom magrittr %>%
#'
#' @examples
find_epsilon <- function(year, sd_1,  sd_2, mean_1_trend, mean_2_trend, mean_1_intercept, mean_2_intercept, pi_1_trend, pi_1_intercept){
  dataset <- tibble(
    year,
    mean_1 = year * mean_1_trend + mean_1_intercept,
    mean_2 = year * mean_2_trend + mean_2_intercept,
    pi_1 = year * pi_1_trend + pi_1_intercept) %>%
    rowwise() %>%
    mutate(
      w = rbinom(1, 1, pi_1), #pi_1 should be a function of year
      u = rnorm(1, mean_1, sd_1), #mean_1 should be a function of year
      v = rnorm(1, mean_2, sd_2), #mean_2 should be a function of year
      epsilon = w * u + (1 - w) * v
    ) %>%
    ungroup()
}


