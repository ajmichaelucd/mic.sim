#' add_covariate
#'
#' adds the covariates based on the covariate_list and input vector
#'
#' @param covariate_list
#' @param input
#'
#' @return
#' @keywords internal
#'
#' @importFrom dplyr as_tibble tibble
#' @importFrom purrr map
#'
#' @examples
add_covariate <- function(covariate_list, input){
  input <- as_tibble(input)
  covariates <- as_tibble(purrr::map(covariate_list, ~draw_covariates(input, .x)),  .name_repair = "unique" )
  names(covariates) <- create_cov_names(covariates)
  tibble(covariates)
}


create_cov_names <- function(df){
  vec = c(1:ncol(df))
  as_vector(map(vec, ~paste("covariate_", .x, sep = "")))
}
