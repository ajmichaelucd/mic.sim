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
check_array_complete <- function(number_of_batches, n_per_row, format, location, array_name, date){
  rows = rep(1:(number_of_batches/n_per_row), each = n_per_row)
  incomplete <- purrr::map2_dbl(1:number_of_batches, rows, ~check_single_run_complete(.x, .y, format, location, array_name, date)) %>%
    stats::na.omit()
if(length(incomplete > 0)){

  return(tibble(incomplete))
} else{print("All Clear")}

  }

check_single_run_complete <- function(i, j, format, location, array_name, date){
   if(format == "name_date_number"){
    prefix <- paste0(array_name, "_row_", j, "_", date)
    run_name <- paste0(prefix, "_run_", i)
  } else{
    errorCondition("Invalid format")
  }

  file <- fs::path(location, run_name, ext = "Rdata")

  if(!file.exists(as.character(file))){
    return(i)
  } else { return(NaN)}
}
