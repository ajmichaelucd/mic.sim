#' Title
#'
#' @param full_results
#'
#' @return
#' @export
#'
#' @importFrom stringr str_detect
#'
#' @examples
add_failure_attr <- function(full_results){
  results <- full_results$result
  attr(results, "survreg_failure") <- any(stringr::str_detect(full_results$warnings, "Ran out of iterations and did not converge"))
  results
}
