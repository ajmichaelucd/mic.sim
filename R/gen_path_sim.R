#' gen_path_sim
#'
#' Creates names of individual batches to load in, useful in combination with a map function and loadRData
#'
#' @param location
#' @param format
#' @param array_name
#' @param date
#' @param i
#'
#' @return
#' @export
#'
#' @importFrom fs path
#'
#' @examples
gen_path_sim <- function(location, format, array_name, date, i){
  if(format == "name_date_number"){
    prefix <- paste(array_name, date, sep = "_")
    run_name <- paste(prefix, i, sep = "_")
  }
  else if(format == "name_number_date"){
    prefix <- paste(array_name, i, sep = "_")
    run_name <- paste(prefix, date, sep = "_")
  }
  else{
    errorCondition("Invalid format")
  }
  fs::path(location, run_name, ext = "Rdata")
}
