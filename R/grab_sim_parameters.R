#' grab_sim_parameters
#'
#' from simulation log
#'
#' @param parameter_log
#' @param array_name
#' @param date
#' @param id_col
#'
#' @return
#' @export
#'
#' @examples
grab_sim_parameters <- function(parameter_log, array_name, date, id_col){
  line <- paste(array_name, date, sep = "_")
  info <- parameter_log %>%
    rename(id = match(id_col, names(parameter_log))) %>%
    filter(id == line)
  info
}
