#' fit_lr
#'
#' @param df data frame of year, observed values (colname is observed_value), and covariates
#' @param MIC_breakpoint on MIC scale, breakpoint for S vs I
#' @param summary whether the output is the summary.glm  (TRUE) or the glm object
#'
#' @return
#' @export
#'
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#' @importFrom broom tidy
#'
#' @examples
fit_lr <- function(observed_value, df, MIC_breakpoint, summary = FALSE){

  n  <- df %>%
    select(starts_with("covariate_")) %>%
    ncol(.)

  outcome <- "dichot"
  variables  <- c("year", paste("covariate_", 1:n, sep = ""))
  f <- as.formula(
    paste(outcome,
          paste(variables, collapse = " + "),
          sep = " ~ "))
  df2 <- df %>%
    mutate(
      dichot = factor(ifelse(observed_value > log2(MIC_breakpoint), "R", "S"), levels = c("S", "R")))

  if(summary == TRUE){
  summary(glm(print(f), data = df2, family = "binomial"))
  }
  else if(summary == "tidy"){
    tidy(glm(print(f), data = df2, family = "binomial"))
  }
else{
  glm(print(f), data = df2, family = "binomial")
}
}
