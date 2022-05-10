#' fit_aft
#'
#' @param df
#' @param covariate_names a vector with all the text names of the covariates (used to build a formula)
#' @param left_bound
#' @param right_bound
#' @param type
#' @param summary
#'
#' @importFrom dplyr mutate all_of select
#' @importFrom magrittr %>%
#' @importFrom survival Surv survreg
#' @importFrom broom tidy
#'
#' @return
#' @export
#'
#' @examples
fit_aft <- function(df,
                    covariate_names,
                    left_bound,
                    right_bound,
                    type,
                    summary = FALSE){

  n  <- df %>%  ###fix this for covariates from real data
    select(all_of(covariate_names)) %>%
    ncol(.)

  if(type %in% c("loglogistic", "weibull", "lognormal", "exponential")){
    outcome <- "surv_object1"
    variables  <- c("year", covariate_names)
    f <- as.formula(
      paste(outcome,
            paste(variables, collapse = " + "),
            sep = " ~ "))
    surv_object1 <- Surv(
      time = ifelse(left_bound == 0, -Inf, left_bound), #because the survreg can't take ln of 0 or negative numbers
      time2 = right_bound,
      type = "interval2")

    if(summary == TRUE){
      summary(survreg(f, data = df, dist = type)) #THIS NEEDS TO VARY FOR COVARIATES
    }
    else if(summary == "tidy"){
      tidy(survreg(f, data = df, dist = type))
    }
    else{
      survreg(f, data = df, dist = type)
    }
  }
  else if(type %in% c("logistic", "gaussian")){
    outcome <- "surv_object1"
    variables  <- c("year", covariate_names)
    f <- as.formula(
      paste(outcome,
            paste(variables, collapse = " + "),
            sep = " ~ "))


    surv_object1 <- Surv(
      time = ifelse(left_bound == 0, -Inf, log2(left_bound)),
      time2 = log2(right_bound),
      type = "interval2")


    if(summary == TRUE){
      summary(survreg(f, data = df, dist = type)) #THIS NEEDS TO VARY FOR COVARIATES
    }
    else if(summary == "tidy"){
      tidy(survreg(f, data = df, dist = type))
    }
    else{
      survreg(f, data = df, dist = type)
    }
  }
  else {
    warning("input error, failed to select from weibull, loglogistic, lognormal, exponential, logistic, gaussian")
  }
}
