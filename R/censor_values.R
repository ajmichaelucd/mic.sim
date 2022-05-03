

#' censor_values
#'
#' adds new columns with left and right censoring indicators, setting up the creation of a Surv object with interval2 format
#'
#' @param observed_value
#' @param low_log_con
#' @param high_log_con
#' @param tested_concentrations
#'
#' @return
#' @export
#'
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#'
#' @examples
censor_values <-
  function(
    observed_value,
#    MIC_breakpoint,
    low_con = 2^-4,
    high_con = 2^4,
    tested_concentrations = log2(low_con):log2(high_con),
    output_scale = "concentration")
  {

    df = dplyr::tibble(
        left_bound = sapply(observed_value, function(x) max(tested_concentrations[tested_concentrations < x])) %>% suppressWarnings(),
        observed_value,
        right_bound = sapply(observed_value, function(x) min(tested_concentrations[tested_concentrations >= x])) %>%
          suppressWarnings(),
        indicator = dplyr::case_when(
          is.finite(right_bound) & is.finite(left_bound) ~ 3,
          is.finite(right_bound) & is.infinite(left_bound) ~ 2,
          is.infinite(right_bound) & is.finite(left_bound) ~ 0
        )
      ) #%>%
#      mutate(
#        dichot = ifelse(observed_value > log2(MIC_breakpoint), "R", "S"))

    if(output_scale == "concentration")
    {
      df =
        df %>%
        mutate(
          left_bound = 2^left_bound,
          right_bound = 2^right_bound
        )
    }

    return(df)

    # df %>%
    #   mutate(
    #     right_bound_1 =
    #       min(tested_concentrations[tested_concentrations > observed_value])
    #
    #     right_bound_1 = ceiling(df$observed_value),
    #     left_bound_1 = ceiling(df$observed_value) - 1
    #   ) %>%
    #   mutate(
    #     left_bound_cens_1 = ifelse(
    #       right_bound_1 <= low_log_con,
    #       -Inf,
    #       ifelse(left_bound_1 > high_log_con, high_log_con,
    #         left_bound_1)
    #     ),
    #     right_bound_cens_1 = ifelse(
    #       right_bound_1 > high_log_con,
    #       Inf,
    #       ifelse(right_bound_1 <= low_log_con, low_log_con,
    #         right_bound_1)
    #     )
    #   ) %>%
    #   mutate(indicator_1 = ifelse(
    #     left_bound_cens_1 == -Inf,
    #     2,
    #     ifelse(right_bound_cens_1 == Inf, 0, 3)
    #   )) %>%
    #   mutate(
    #     left_bound_cens_1_exp = exp(left_bound_cens_1),
    #     right_bound_cens_1_exp = exp(right_bound_cens_1)
    #   ) %>%
    # mutate(dichot = ifelse(observed_value > log2(MIC_breakpoint), "R", "S")) %>%
    # mutate(year_sq = year ^ 2)
  }