#' fit_spaft
#'
#' apply semiparametric accelerated failure time model using smoothSurv
#'
#' @param observed_values
#' @param covariate_data_frame
#' @param low_con
#' @param high_con
#' @param tested_concentrations
#' @param summary
#'
#' @return
#' @export
#'
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#' @importFrom survival Surv
#' @importFrom smoothSurv smoothSurvReg
#' @importFrom broom tidy
#'
#' @examples
fit_spaft <- function(observed_values,
                      covariate_data_frame,
                      low_con = 2^-4,
                      high_con = 2^4,
                      tested_concentrations = log2(low_con):log2(high_con),
                      summary = FALSE){

  n  <- covariate_data_frame %>%
    select(starts_with("covariate_")) %>%
    ncol(.)

  outcome <- "surv_object1"
  #variables <-
  variables  <- c("year", paste("covariate_", 1:n, sep = ""))
  f <- as.formula(
    paste(outcome,
          paste(variables, collapse = " + "),
          sep = " ~ "))
  df <- censor_values(observed_values,
                      low_con,
                      high_con,
                      tested_concentrations,
                      output_scale = "concentration") %>%
    merge(., covariate_data_frame) %>%
    tibble()
  surv_object1 <- Surv(
    time = ifelse(df$left_bound == 0, 0.000000000000000000000000001, df$left_bound), #because the survreg can't take ln of 0 or negative numbers
    time2 = df$right_bound,
    type = "interval2")

if(summary == TRUE){
  summary(smoothSurvReg(print(f), data = df)) #THIS NEEDS TO VARY FOR COVARIATES
}
  else if(summary == "tidy"){
    tidy(smoothSurvReg(print(f), data = df))
  }
  else{
    smoothSurvReg(print(f), data = df)
  }

}
