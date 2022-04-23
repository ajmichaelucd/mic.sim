#' fit_aft
#'
#' @param observed_values
#' @param type
#' @param MIC_breakpoint
#' @param low_con
#' @param high_con
#' @param tested_concentrations
#'
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#' @importFrom survival Surv survreg
#'
#' @return
#' @export
#'
#' @examples
fit_aft <- function(observed_values,
                    covariate_data_frame,
                    type,
                    MIC_breakpoint,
                    low_con = 2^-4,
                    high_con = 2^4,
                    tested_concentrations = log2(low_con):log2(high_con)){

  if(type %in% c("loglogistic", "weibull", "lognormal", "exponential")){
    outcome <- "surv_object1"
    #variables <-
    variables  <- c("year", paste("covariate_", 1:length(covariate_list), sep = ""))
    f <- as.formula(
      paste(outcome,
            paste(variables, collapse = " + "),
            sep = " ~ "))
    df <- censor_values(observed_values, MIC_breakpoint,
                        low_con,
                        high_con,
                        tested_concentrations,
                        output_scale = "concentration") %>%   ####Need to add covariates back in here before the model is made
merge(., covariate_data_frame) %>%
  tibble()
    surv_object1 <- Surv(
      time = ifelse(df$left_bound == 0, 0.000000000000000000000000001, df$left_bound), #because the survreg can't take ln of 0 or negative numbers
      time2 = df$right_bound,
      type = "interval2")


    summary(survreg(print(f), data = df, dist = type)) #THIS NEEDS TO VARY FOR COVARIATES

  }
  else if(type %in% c("logistic", "gaussian")){
    outcome <- "surv_object1"
    variables  <- c("year", paste("covariate_", 1:length(covariate_list), sep = ""))
    f <- as.formula(
      paste(outcome,
            paste(variables, collapse = " + "),
            sep = " ~ "))

    df <- censor_values(observed_values, MIC_breakpoint,
                        low_con,
                        high_con,
                        tested_concentrations,
                        output_scale = "log") %>%   ####Need to add covariates back in here before the model is made
  merge(., covariate_data_frame) %>%
  tibble()
    surv_object1 <- Surv(
      time = df$left_bound,
      time2 = df$right_bound,
      type = "interval2")


    summary(survreg(print(f), data = df, dist = type)) #THIS NEEDS TO VARY FOR COVARIATES

  }
  else {
    warning("input error, failed to select from weibull, loglogistic, lognormal, exponential, logistic, gaussian")
  }
}
