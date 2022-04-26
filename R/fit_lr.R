#' fit_lr
#'
#' @param df data frame of year, observed values (colname is observed_value), and covariates
#' @param MIC_breakpoint on MIC scale, breakpoint for S vs I
#'
#' @return
#' @export
#'
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#'
#' @examples
fit_lr <- function(df, MIC_breakpoint){

  outcome <- "dichot"
  variables  <- c("year", paste("covariate_", 1:length(covariate_list), sep = ""))
  f <- as.formula(
    paste(outcome,
          paste(variables, collapse = " + "),
          sep = " ~ "))
  df2 <- df %>%
    mutate(
      dichot = factor(ifelse(observed_value > log2(MIC_breakpoint), "R", "S"), levels = c("S", "R")))

  summary(glm(print(f), data = df2, family = "binomial"))

}
