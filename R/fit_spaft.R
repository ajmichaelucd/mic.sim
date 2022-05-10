#' fit_spaft
#'
#' apply semiparametric accelerated failure time model using smoothSurv
#'
#' @param df
#' @param covariate_names a vector with all the text names of the covariates (used to build a formula)
#' @param left_bound
#' @param right_bound
#' @param summary
#'
#' @return
#' @export
#'
#' @importFrom dplyr mutate all_of select
#' @importFrom magrittr %>%
#' @importFrom survival Surv
#' @importFrom smoothSurv smoothSurvReg
#' @importFrom broom tidy
#'
#' @examples
fit_spaft <- function(df,
                      covariate_names,
                      left_bound,
                      right_bound,
                      summary = FALSE){

  n  <- df %>%  ###fix this for covariates from real data
    select(all_of(covariate_names)) %>%
    ncol(.)

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
  summary(smoothSurvReg(f, data = df)) #THIS NEEDS TO VARY FOR COVARIATES
}
  else if(summary == "tidy"){
    tidy(smoothSurvReg(f, data = df))
  }
  else{
    smoothSurvReg(f, data = df)
  }

}
