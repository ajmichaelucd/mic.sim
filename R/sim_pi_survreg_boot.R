#' Title
#'
#' @param df
#' @param fit
#' @param alpha
#' @param nSims
#' @param distr
#'
#' @return
#' @export
#'
#' @importFrom MASS mvrnorm
#'
#' @examples
sim_pi_survreg_boot = function (df, fit, alpha = 0.05, nSims = 10000)
{
  distr <- fit$dist
  out <- predict(fit, newdata = df, type = "quantile", p = 0.5)
  params <- sim_surv_coefs(df = df, fit = fit, nSims = nSims, distr)
  sim_response <- get_sim_response_surv_boot(df, fit, params, distr)
  lwr <- apply(sim_response, 1, FUN = quantile, probs = alpha/2,
               type = 1)
  upr <- apply(sim_response, 1, FUN = quantile, probs = 1 -
                 alpha/2, type = 1)
  #df %>% mutate(out, lwr, upr) %>% return
  df %>% mutate(lwr, upr) %>% return()
}

sim_surv_coefs = function (df, fit, nSims, distr)
{
  if (distr == "exponential") {
    vcov.hat <- vcov(fit)
  }
  else {
    vcov.hat <- vcov(fit)
  }
  beta.hat <- coef(fit)
  params <- matrix(NA, nrow = nSims, ncol = length(beta.hat))
  params <- MASS::mvrnorm(nSims, beta.hat, vcov.hat[1:length(beta.hat), 1: length(beta.hat)]) ##read up to see if p-spline coefs are approx normal
  params
}

get_sim_response_surv_boot = function (df, fit, params, distr)
{
  nSims <- dim(params)[1]
  nPreds <- NROW(df)
  modmat <- model.matrix(fit, data = df)
  sim_response <- matrix(0, ncol = nSims, nrow = nPreds)
  scale <- fit$scale
  for (i in 1:nSims) {
    linear_pred <- modmat %*% params[i, 1:dim(params)[2]]
    if ((distr == "lognormal") || (distr == "loggaussian")) {
      sim_response[, i] <- exp(linear_pred + scale * rnorm(n = nPreds))
    }
    if (distr == "weibull") {
      sim_response[, i] <- exp(linear_pred + scale * rsev(n = nPreds))
    }
    if (distr == "exponential") {
      sim_response[, i] <- exp(linear_pred + rsev(n = nPreds))
    }
    if (distr == "loglogistic") {
      sim_response[, i] <- exp(linear_pred + scale * rlogis(n = nPreds))
    }
    if((distr == "gaussian") || (distr == "normal")) {
      sim_response[, i] <- (linear_pred + scale * rnorm(n = nPreds))
    }
  }
  sim_response %>% return()
}
