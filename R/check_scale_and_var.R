#' Title
#'
#' @param output
#'
#' @importFrom purrr pmap
#'
#' @return
#' @export
#'
#' @examples
check_scale_and_var = function(output) {
  ub =
    case_when(
      max(output$possible_data$right_bound) == Inf ~ max(output$possible_data$left_bound),
      max(output$possible_data$right_bound) != Inf ~ max(output$possible_data$right_bound)
    )



  lb =
    case_when(
      attr(output$possible_data, "scale") == "log" &
        min(output$possible_data$left_bound) == -Inf ~ min(output$possible_data$right_bound),
      attr(output$possible_data, "scale") == "log" &
        min(output$possible_data$left_bound) != -Inf ~ min(output$possible_data$left_bound),
      attr(output$possible_data, "scale") == "MIC" &
        min(output$possible_data$left_bound) == 0 ~ min(output$possible_data$right_bound),
      attr(output$possible_data, "scale") == "MIC" &
        min(output$possible_data$left_bound) > 0 ~ min(output$possible_data$left_bound),
      TRUE ~ NaN
    )

  range = ub - lb




  fitted_comp = case_when(
    is.null(output$fixed_side) ~ 1:output$ncomp,!is.null(output$fixed_side) &&
      output$fixed_side == "RC" ~ 1,!is.null(output$fixed_side) &&
      output$fixed_side == "LC" ~ 2,
    TRUE ~ -1
  ) %>% unique


  scale_df = tibble(scale = map_dbl(output$mu_model, get_scale), fitted_comp) %>% mutate(range)
  scale_df %>%
    pmap(., message_scale_check)




  var_df = map_dfr(output$mu_model, get_avg_var) %>%
    mutate(fitted_comp) %>% mutate(guideline = range * 10,
                                   var_check = var > guideline)

  var_df %>% pmap(., message_var_check)

  left_join(var_df, scale_df, by = "fitted_comp") %>% select(-c(guideline, var_check)) %>% rename(
    Component = fitted_comp,
    `Avg Variance of Covariates` = var,
    `Component Width (sigma)` = scale,
    `MIC Range` = range
  ) %>%
    relocate(Component, `Avg Variance of Covariates`, everything()) %>% return()
}





get_scale = function(mu_model) {
  mu_model$scale %>% return()
}

get_avg_var = function(mu_model) {
  tibble(var = diag(mu_model$var) %>% mean) %>% return()
}

message_scale_check = function(scale, fitted_comp, range, ...) {
  if (scale > (2 * range)) {
    message(
      paste0(
        "Width of component ",
        fitted_comp,
        " is more than twice as wide as the range of MICs"
      )
    )
  } else{
    message(
      paste0(
        "Width of component ",
        fitted_comp,
        " is within a factor of two of the range of the MICs"
      )
    )
  }
}

message_var_check = function(fitted_comp, var_check, ...) {
  if (var_check == 0) {
    message(
      paste0(
        "Variance of covariates for component ",
        fitted_comp,
        " is within a factor of 10 of range of log2 MICs"
      )
    )
  } else if (var_check == 1) {
    message(
      paste0(
        "The average variance of covariates for component ",
        fitted_comp,
        " is more than ten times the range of log2 MICs, suggesting mu model fit for this component is unreliable"
      )
    )
  } else{
    errorCondition("Check variances failed")
  }
}
