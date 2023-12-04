#' Fit Pi Model
#'
#' Fits a weighted binomial model using either a logit or identity function to estimate covariate effects on component weights, pi. Uses the gam function from the gam package.
#'
#' @param pi_formula formula used to fit the binomial model for pi, the component weights. The left hand side should designate the level of c, component to classify as an event, and the right hand side should include the covariates. Accepts smoothing splines s() and local regression lo() from the gam package. Example of formula: c == "2" ~ s(t)
#' @param pi_link string specifying whether to use a logit or identity link function. Possible options are "logit" and "identity"
#' @param possible_data data frame iterated on by the EM algorithm, should contain left_bound, right_bound, c, P(C=c|y,t), and any covariates, such as time, e.g. "t"
#'
#' @return
#' @export
#'
#' @examples
fit_pi_model = function(pi_formula, pi_link, possible_data){

  if(pi_link == "logit"){
    binom_model = gam::gam(pi_formula, family = binomial(link = "logit"), data = possible_data, weights = `P(C=c|y,t)`) %>% suppressWarnings()
  } else if(pi_link == "identity"){

    binom_model = gam::gam(pi_formula, family = binomial(link = "identity"), data = possible_data, weights = `P(C=c|y,t)`) %>% suppressWarnings()
  } else{ errorCondition("pick logit or identity link function")}

  return(binom_model)
}
