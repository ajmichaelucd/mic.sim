#' fit_spaft
#'
#' apply semiparametric accelerated failure time model using smoothSurv
#'
#' @param df
#' @param time
#' @param covariate_names a vector with all the text names of the covariates (used to build a formula)
#' @param left_bound
#' @param right_bound
#' @param summary
#'
#' @return
#' @export
#'
#' @importFrom dplyr mutate all_of select case_when
#' @importFrom magrittr %>%
#' @importFrom survival Surv
#' @importFrom smoothSurv smoothSurvReg
#' @importFrom broom tidy
#'
#' @examples
fit_spaft <- function(df,
                      time = "t",
                      covariate_names,
                      left_bound,
                      right_bound,
                      summary = FALSE){

  n  <- df %>%  ###fix this for covariates from real data
    select(all_of(covariate_names)) %>%
    ncol(.)

  outcome <- "surv_object1"
  variables  <- case_when(is.null(time) & is.null(covariate_names) ~ "1",
                          TRUE ~ c(time, covariate_names))
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
