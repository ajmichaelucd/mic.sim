#' error_measures_one_batch
#'
#' @param location
#' @param format
#' @param array_name
#' @param date
#' @param i
#' @param intercepts
#' @param trends
#' @param sigma
#' @param pi
#' @param sigma_tolerance
#' @param pi_tolerance
#' @param intercepts_tolerance
#' @param trends_tolerance
#'
#' @return
#' @export
#'
#' @importFrom data.table rbindlist
#' @importFrom dplyr tibble mutate
#' @importFrom magrittr %>%
#' @importFrom tidyr pivot_longer pivot_wider separate
#' @importFrom fs path
#'
#' @examples
error_measures_one_batch <- function(location, format, array_name, date, i, intercepts, trends, sigma, pi, sigma_tolerance = 100, pi_tolerance = 100, intercepts_tolerance = 100, trends_tolerance = 100 ){
  file  <- gen_path_sim(location = location, format = format, array_name = array_name, date = date, i = i)
  results <- loadRData(file)



  map(results, ~error_measures_one_run_both_directions(.x, intercepts, trends, sigma, pi, sigma_tolerance, pi_tolerance, intercepts_tolerance, trends_tolerance)) %>%
    data.table::rbindlist()

}
