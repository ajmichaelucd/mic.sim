#' calculate_single_covariate_effect
#'
#' used in the find_covariate_effects function on each covariate to find the individual covariate effect for each observation in simulated data
#'
#' @param covariate_entry_column
#' @param covariate_list_of_lists
#'
#' @return
#' @export
#'
#' @importFrom dplyr tibble left_join select %>%
#'
#' @examples
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
    else {warningCondition("typo")}
  }
