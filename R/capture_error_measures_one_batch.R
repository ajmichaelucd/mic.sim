#' Title
#'
#' @param location
#' @param format
#' @param array_name
#' @param date
#' @param i
#' @param number_of_batches
#'
#' @return
#' @export
#'
#' @examples
capture_error_measures_one_batch <- function(location,
                                                 format,
                                                 array_name,
                                                 date,
                                                 i,
                                                 number_of_batches){
  ###Load in a batch--------------------------
  file  <- gen_path_sim(location = location, format = format, array_name = array_name, date = date, i = i)
  batch_results <- loadRData(file)



  ###Visualize Progress-----------------------
  hits = 1:100
  scaled = number_of_batches/100
  reports = round(scaled * hits)



  if(i %in% reports){
    a <- paste(rep("|", round(i / scaled)), collapse = "")
    b <- paste(rep(".", 100 - round(i/scaled)), collapse = "")

    paste0(paste0(a, b), "*") %>% print()
  }


  ###Analysis of iterations---------------------
  purrr::map(batch_results$model_results, ~capture_error_measures_one_run(results =  .x, settings = batch_results$settings)) %>%
    data.table::rbindlist()
  ##add attribute read to the error_measures_one_run_both_directions segment

}
