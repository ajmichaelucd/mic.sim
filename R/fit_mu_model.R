#' Fit Mu Model
#'
#' Function to filter the data to the desired component using the variable c and fit survreg to the data using a formula supplied by the user, typically passed in through the EM algorithm
#'
#' @param possible_data data frame iterated on by the EM algorithm, should contain left_bound, right_bound, c, P(C=c|y,t), and any covariates, such as time, e.g. "t"
#' @param pred_comp numeric variable, which component is this model being fitted to, e.g. 2
#' @param mu_formula formula being used to fit the model for mu, should contain a Surv object for an outcome and any covariates of interest, accepts pspline and other arguments to survreg
#' @param maxiter_survreg maximum number of iterations for survreg to fit the model
#'
#' @return
#' @keywords internal
#'
#' @examples
fit_mu_model = function(possible_data, pred_comp, mu_formula, maxiter_survreg = 30){
  mu = possible_data %>% filter(`P(C=c|y,t)` > 0 & c == pred_comp) %>%
    mutate(`P(C=c|y,t)` =  paste0(`P(C=c|y,t)`) %>% as.numeric) %>%
    survreg(
      formula = mu_formula,
      weights = `P(C=c|y,t)`,
      data = .,
      dist = "gaussian",
      control = survreg.control(maxiter = maxiter_survreg)) %>% return()
}

#survreg_safe = purrr::safely(survival::survreg, otherwise = "Error")
fit_mu_model_safe = purrr::safely(fit_mu_model, otherwise = "Error")

fit_mu_model_safe_formatted = function(possible_data, pred_comp, mu_formula, maxiter_survreg = 30){
  mu = fit_mu_model_safe(possible_data, pred_comp, mu_formula, maxiter_survreg = 30)
  mu_model = reformat_safe(mu)
  return(mu_model)
}

