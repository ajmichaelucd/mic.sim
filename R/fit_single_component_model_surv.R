#' Title
#'
#' @param visible_data
#' @param mu_formula
#' @param maxiter_survreg
#' @param verbose
#'
#' @return
#' @keywords internal
#'
#' @examples
fit_single_component_model_surv = function(visible_data, mu_formula, maxiter_survreg, verbose){

  mu_model  <- survival::survreg(
    mu_formula,  ##Make this chunk into an argument of the function
    data = visible_data,
    dist = "gaussian",
    control = survreg.control(maxiter = maxiter_survreg, debug = verbose > 3))

  return(list(possible_data = visible_data,
              mu_model = mu_model,
              converge = "YES",
              ncomp = ncomp,
         likelihood = tibble(step = 1, likelihood = mu_model$loglik[2]))
  )
}
