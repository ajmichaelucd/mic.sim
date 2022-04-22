
#' censor_values
#'
#' adds new columns with left and right censoring indicators, setting up the creation of a Surv object with interval2 format
#'
#' @param df must contain a column named "observed value" which contains the uncensored simulated values
#' @param MIC_breakpoint is the highest MIC that is still S (the MIC where susceptible is defined as ≥ MIC), this value is on the MIC scale (we take log2 of MIC_breakpoint before applying it to the continuous values that are simulated on the log2 scale)
#'
#' @return
#' @export
#'
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#'
#' @examples
censor_values <- function(df, MIC_breakpoint, low_log_con = -4, high_log_con = 4){
  df %>%
    mutate(right_bound_1 = ceiling(df$observed_value),
           left_bound_1 = ceiling(df$observed_value) - 1) %>%
    mutate(left_bound_cens_1 = ifelse(right_bound_1 <= low_log_con, -Inf, ifelse(left_bound_1 > high_log_con, high_log_con,
                                                                                 left_bound_1)),
           right_bound_cens_1 = ifelse(right_bound_1 > high_log_con, Inf, ifelse(right_bound_1 <= low_log_con, low_log_con,
                                                                                 right_bound_1))) %>%
    mutate(indicator_1 = ifelse(left_bound_cens_1 == -Inf, 2,
                                ifelse(right_bound_cens_1 == Inf, 0, 3))) %>%
    mutate(left_bound_cens_1_exp = exp(left_bound_cens_1),
           right_bound_cens_1_exp = exp(right_bound_cens_1)) %>%
    mutate(dichot = ifelse(observed_value > log2(MIC_breakpoint), "R", "S")) %>%
    mutate(year_sq = year^2)
}


