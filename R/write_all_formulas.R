#' Write Formulas for Mu Models
#'
#' Used to write the formulas for mu models, returns either a single formula or a list of formulas.
#'
#' @inheritParams fit_EM
#'
#' @param non_linear_term
#' @param degrees Vector of length equal to the number of component means being estimated. Elements are numeric and correspond to the number of degrees (polynomial) or degrees of freedom (pspline) in each mu model. First element corresponds to lowest component, last element corresponds to highest component.
#' @param covariates
#' @param model String, "pspline" or "polynomial". Which non-linear term should be used in model
#'
#' @return
#' @export
#'
#' @examples
write_all_formulas = function(non_linear_term, degrees, covariates, model){
  if(model == "polynomial"){
    purrr::map(degrees, ~ write_single_formula.polynomial(non_linear_term, .x, covariates)) %>% return()

  }else if(model == "surv" | model == "pspline"){
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
