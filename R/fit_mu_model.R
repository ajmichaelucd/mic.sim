#' Fit Mu Model
#'
#' Function to filter the data to the desired component using the variable c and fit survreg to the data using a formula supplied by the user, typically passed in through the EM algorithm
#'
#' @param possible_data data frame iterated on by the EM algorithm, should contain left_bound, right_bound, c, & P(C=c|y,t)
#' @param comp numeric variable, which component is this model being fitted to, e.g. 2
#' @param mu_formula formula being used to fit the model for mu, should contain a Surv object for an outcome and any covariates of interest, accepts pspline and other arguments to survreg
#' @param maxiter_survreg maximum number of iterations for survreg to fit the model
#'
#' @return
#' @export
#'
#' @examples
fit_mu_model = function(possible_data, comp, mu_formula, maxiter_survreg = 30){
  possible_data %>% filter(`P(C=c|y,t)` > 0 & c == comp) %>%
    survival::survreg(
      formula = mu_formula,
      weights = `P(C=c|y,t)`,
      data = .,
      dist = "gaussian",
      control = survreg.control(maxiter = maxiter_survreg, debug = verbose > 3)) %>%
    return()
}
