#' fit_lr
#'
#' @param df data frame of year, observed values (colname is observed_value), and covariates
#' @param time
#' @param covariate_names
#' @param right_bound
#' @param MIC_breakpoint on MIC scale, highest value that is still S (so if the guide says all organisms with an MIC ≤ 2  are S and ≥ 4 are R, the breakpoint is 2)
#' @param summary whether the output is the summary.glm  (TRUE) or the glm object
#'
#' @return
#' @export
#'
#' @importFrom dplyr mutate case_when
#' @importFrom magrittr %>%
#' @importFrom broom tidy
#'
#' @examples
fit_lr <- function(df, time = "t", covariate_names, left_bound, right_bound, MIC_breakpoint, summary = FALSE){

  n  <- df %>%  ###fix this for covariates from real data
    select(all_of(covariate_names)) %>%
    ncol(.)

  outcome <- "dichot"
  variables  <- case_when(is.null(time) & is.null(covariate_names) ~ "1",
                          TRUE ~ c(time, covariate_names))
  f <- as.formula(
    paste(outcome,
          paste(variables, collapse = " + "),
          sep = " ~ "))
  df2 <- df %>%
    mutate(
      dichot = factor(ifelse(right_bound > log2(MIC_breakpoint), "R", "S"),  levels = c("S", "R"))
      )


  if(summary == TRUE){
  summary(glm(f, data = df2, family = "binomial"))
  }
  else if(summary == "tidy"){
    tidy(glm(f, data = df2, family = "binomial"))
  }
else{
  glm(f, data = df2, family = "binomial")
}
}
