#' Title
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
#' @examples
capture_error_measures_one_batch <- function(location,
                                             format,
                                             array_name,
                                             date,
                                             i,
                                             batch_size,
                                             intercepts,
                                             trends,
                                             sigma,
                                             pi_int,
                                             pi_trend,
                                             sigma_tolerance = c(.05, 100),
                                             pi_tolerance = c(.05, .95),
                                             intercepts_tolerance = 100,
                                             trends_tolerance = 100,
                                             number_of_batches
){
  file  <- gen_path_sim(location = location, format = format, array_name = array_name, date = date, i = i)
  results <- loadRData(file)
  # results_pre <- sticky::sticky(results_pre)

  #attr_print  <- function(run){
  #    print(attr(run, "survreg_failure"))
  #}


  hits = 1:100
  scaled = number_of_batches/100
  reports = round(scaled * hits)



    if(i %in% reports){
      a <- paste(rep("|", round(i / scaled)), collapse = "")
      b <- paste(rep(".", 100 - round(i/scaled)), collapse = "")

      paste0(paste0(a, b), "*") %>% print()
    }






  #results_pre_attr <- map(results_pre, attr_append)

  #results <- purrr::map2(1:batch_size, results_pre, ~append(.y, (batch_size*(i - 1) + .x )))

  #note results[[3]] failure is not list object

  purrr::map(results, ~capture_error_measures_one_run(.x, intercepts, trends, sigma, pi_int, pi_trend, sigma_tolerance, pi_tolerance, intercepts_tolerance, trends_tolerance)) %>%
    data.table::rbindlist()
  ##add attribute read to the error_measures_one_run_both_directions segment

}

#attr_append <- function(run){
#  append(run, attr(run, "survreg_failure"))
#}

