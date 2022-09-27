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
error_measures_one_batch <- function(location, format, array_name, date, i, intercepts, trends, sigma){
  file  <- gen_path_sim(location = location, format = format, array_name = array_name, date = date, i = i)
  results <- loadRData(file)



  map(results, ~error_measures_one_run(.x, intercepts, trends, sigma)) %>%
    data.table::rbindlist()

}
