#' Fit All Mu Models
#'
#' Loops the function that fits survreg to each component over all the iterations (1 through the number of components being fitted by the EM algorithm)
#'
#' @param possible_data data frame iterated on by the EM algorithm, should contain left_bound, right_bound, c, P(C=c|y,t), and any covariates, such as time, e.g. "t"
#' @param ncomp number of components being fitted by the EM algorithm
#' @param mu_formula formula being used to fit the model for mu, should contain a Surv object for an outcome and any covariates of interest, accepts pspline and other arguments to survreg
#' @param maxiter_survreg maximum number of iterations for survreg to fit the model
#'
#' @return
#' @export
#'
#' @examples
fit_all_mu_models.surv = function(possible_data, ncomp, mu_formula, maxiter_survreg){
  mu_models_new = purrr::map(1:ncomp, ~fit_mu_model(possible_data = possible_data, pred_comp = .x, mu_formula = mu_formula, maxiter_survreg = maxiter_survreg))
  mu_models_new = purrr::map(mu_models_new, ~set_model_attr(.x, possible_data))
  attr(mu_models_new, "model") <- attr(possible_data, "model")
  return(mu_models_new)
  }
