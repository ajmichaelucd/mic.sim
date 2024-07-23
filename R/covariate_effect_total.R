#' covariate_effect_total
#'
#' uses model.matrix to
#'
#' @param data_tibble
#' @param covariate_effect_vector
#'
#' @return
#' @keywords internal
#'
#' @importFrom dplyr select starts_with
#' @importFrom magrittr %>%
#'
#' @examples
covariate_effect_total <- function(data_tibble, covariate_effect_vector){

  f <- data_tibble %>%
    select(starts_with("covariate"))


  model.matrix(data = f, ~.) %*% covariate_effect_vector
}
