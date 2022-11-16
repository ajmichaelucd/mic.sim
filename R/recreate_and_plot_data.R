#' recreate_and_plot_data
#'
#' @param set_numbers
#' @param n
#' @param intercepts
#' @param trends
#' @param sigma
#' @param pi
#' @param nyears
#' @param covariate_list
#' @param covariate_effect_vector
#' @param low_con
#' @param high_con
#' @param scale
#' @param converge_incorrectly_vector
#' @param failure_to_converge_vector
#'
#' @return
#' @export
#'
#' @examples
recreate_and_plot_data <- function(set_numbers, n, intercepts, trends, sigma, pi, nyears, covariate_list, covariate_effect_vector, low_con, high_con, scale, converge_incorrectly_vector, failure_to_converge_vector){
  i = set_numbers

  data_sets <- purrr::map(i, ~recreate_data_set(i = .x, n = n, intercepts = intercepts, trends = trends, sigma = sigma, pi = pi, nyears = nyears, low_con = low_con, high_con = high_con, scal = scale))


  plot_data_sets(data_sets = data_sets, converge_incorrectly_vector = converge_incorrectly_vector, failure_to_converge_vector = failure_to_converge_vector, low_con = low_con, high_con = high_con)

}
