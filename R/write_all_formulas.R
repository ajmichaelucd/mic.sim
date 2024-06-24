#' Title
#'
#' @param non_linear_term
#' @param degrees
#' @param covariates
#' @param model
#'
#' @return
#' @export
#'
#' @examples
write_all_formulas = function(non_linear_term, degrees, covariates, model){
  if(model == "polynomial"){
    purrr::map(degrees, ~ write_single_formula.polynomial(non_linear_term, .x, covariates)) %>% return()

  }else if(model == "surv"){
    purrr::map(degrees, ~ write_single_formula.surv(non_linear_term, .x, covariates)) %>% return()
  }else{
    errorCondition("Please use 'surv' or 'polynomial' for model")
  }
}

write_single_formula.surv = function(non_linear_term, degrees, covariates){
  reformulate(
    termlabels = c(
      paste0("pspline(", non_linear_term, ",", "df = ", degrees, ")"),
      covariates
    ),
    response = "Surv(time = left_bound, time2 = right_bound, type = 'interval2')"
  )
}

write_single_formula.polynomial = function(non_linear_term, degrees, covariates){
  reformulate(
    termlabels = c(
      paste0("poly(", non_linear_term, ",", "degree = ", degrees, ")"),
      covariates
    ),
    response = "Surv(time = left_bound, time2 = right_bound, type = 'interval2')"
  )
}
