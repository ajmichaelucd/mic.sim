#' add_covariate
#'
#' adds the covariates based on the covariate_list and year vector
#'
#' @param covariate_list
#' @param year
#'
#' @return
#' @export
#'
#' @importFrom dplyr as_tibble tibble
#' @importFrom purrr map
#'
#' @examples
add_covariate <- function(covariate_list, year){
  year <- as_tibble(year)
  covariates <- as_tibble(purrr::map(covariate_list, ~draw_covariates(year, .x)),  .name_repair = "unique" )
  names(covariates) <- create_cov_names(covariates)
  tibble(covariates)
}
