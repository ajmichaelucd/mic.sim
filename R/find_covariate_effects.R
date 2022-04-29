#' find_covariate_effects
#'
#' uses map from purrr package to find the single covariate effect over each covariate
#'
#' @param input simulated data goes here, first two columns of that should be year and base data
#' @param cov_list this is the covariate list where each entry is a list of 2 vectors, the first describes the distribution of the variable and the second describes its effects
#'
#'
#' @return
#' @export
#'
#' @importFrom purrr map2
#' @importFrom dplyr select %>%
#'
#' @examples
find_covariate_effects <- function(input, cov_list){
  covariate_entries <- input %>%
    select(-(1:2))

  purrr::map2(covariate_entries, cov_list, ~calculate_single_covariate_effect(.x, .y))


}
