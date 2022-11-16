#' check_array_complete
#'
#' @param number_of_batches
#' @param format
#' @param location
#' @param array_name
#' @param date
#'
#' @importFrom magrittr %>%
#' @importFrom purrr map_dbl
#' @importFrom stats na.omit
#' @importFrom fs path
#'
#' @return
#' @export
#'
#' @examples
check_array_complete <- function(number_of_batches, format, location, array_name, date){
  incomplete <- purrr::map_dbl(1:number_of_batches, ~check_single_run_complete(.x, format, location, array_name, date)) %>%
    stats::na.omit()
if(length(incomplete > 0)){

  return(tibble(incomplete))
} else{print("All Clear")}

  }

check_single_run_complete <- function(i, format, location, array_name, date){
  if(format == "name_date_number"){
    prefix <- paste(array_name, date, sep = "_")
    run_name <- paste(prefix, i, sep = "_")
  } else if(format == "name_number_date"){
    prefix <- paste(array_name, i, sep = "_")
    run_name <- paste(prefix, date, sep = "_")
  } else{
    errorCondition("Invalid format")
  }

  file <- fs::path(location, run_name, ext = "Rdata")

  if(!file.exists(as.character(file))){
    return(i)
  } else { return(NaN)}
}
