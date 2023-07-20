#' Title
#'
#' @param results
#' @param directionality
#' @param analysis
#' @param single_model_output
#'
#' @return
#' @export
#'
#' @examples
scale_select <- function(results, directionality, analysis, single_model_output) {
  if (analysis == "fm") {
    if (directionality %>% pull(flip_decision) == "no flip") {
      tibble(
        c1_scale = single_model_output$newmodel[[1]]$scale,
        c2_scale = single_model_output$newmodel[[2]]$scale
      ) %>% return()
    } else{
      tibble(
        c1_scale = results$single_model_output$newmodel[[2]]$scale,
        c2_scale = results$single_model_output$newmodel[[1]]$scale
      ) %>% return()
    }
  } else if (analysis == "fms") {
    tibble(c1_scale = results$single_model_output$newmodel$scale,
           c2_scale = NaN) %>% return()
  } else{
    scale_select <- tibble(c1_scale = NaN,
                           c2_scale = NaN)
  }
}
