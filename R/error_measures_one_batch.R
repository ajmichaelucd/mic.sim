#' error_measures_one_batch
#'
#' @param location
#' @param format
#' @param array_name
#' @param date
#' @param i
#' @param batch_size
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
#' @importFrom purrr map map2
#' @importFrom dplyr tibble mutate
#' @importFrom magrittr %>%
#' @importFrom tidyr pivot_longer pivot_wider separate
#' @importFrom fs path
#'
#' @examples
error_measures_one_batch <- function(location,
                                     format,
                                     array_name,
                                     date,
                                     i,
                                     batch_size,
                                     intercepts,
                                     trends,
                                     sigma,
                                     pi,
                                     sigma_tolerance = c(.05, 100),
                                     pi_tolerance = c(.05, .95),
                                     intercepts_tolerance = 100,
                                     trends_tolerance = 100
){
  file  <- gen_path_sim(location = location, format = format, array_name = array_name, date = date, i = i)
  results_pre <- loadRData(file)

  results <- purrr::map2(1:batch_size, results_pre, ~append(.y, (i*(batch_size - 1) + .x )))


  purrr::map(results, ~error_measures_one_run_both_directions(.x, intercepts, trends, sigma, pi, sigma_tolerance, pi_tolerance, intercepts_tolerance, trends_tolerance)) %>%
    data.table::rbindlist()

}
