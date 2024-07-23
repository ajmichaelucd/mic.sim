#' Intial Weighting Scheme: Fixed Regression at Boundaries
#'
#' Fits regression lines with slope = 0 at high_con and low_con with sigma values of 0.2 times the difference between high_con and low_con, and calculates initial weights from this
#'
#' @param visible_data the data frame passed into the EM algorithm, at minimum consists of: an id column, a time column, left_bound, right_bound, low_con, and high_con
#' @param ncomp number of components in the model being fitted, this weighting scheme only is valid for 2 component models
#'
#' @return
#' @keywords internal
#'
#' @examples
initial_weighting_fixed_regression_at_boundaries = function(visible_data, ncomp, sd_parameter = 0.2){

  if(ncomp != 2){
    errorCondition("This initial weighting scheme is appropriate for 2 component models")
  }

  visible_data <- visible_data %>% mutate(cens = case_when(
    left_bound == -Inf | right_bound == low_con ~ "lc",
    right_bound == Inf |
      left_bound == high_con ~ "rc",
    TRUE ~ "int"
  ))
  n_obs <- nrow(visible_data)

  full_set = tibble(
    cens = c("rc", "lc", "int")
  )

  cens_counts <-
    visible_data %>%
    summarize(.by = cens,
              n = n()
    ) %>% right_join(., full_set, by = join_by(cens)) %>% mutate(n = case_when(
      is.na(n) ~ 0,
      TRUE ~ n
    )) %>% pivot_wider(
      names_from = cens, values_from = n
    )

  lc <- cens_counts %>% pull(lc)
  int <- cens_counts %>% pull(int)
  rc <- cens_counts %>% pull(rc)
  `P(1)` <- (lc + (0.5 * int) + 1) / (n_obs + 2)
  `P(2)` <- (rc + (0.5 * int) + 1) / (n_obs + 2)

  visible_data %>%
    reframe(.by = everything(),
            c = as.character(1:2)
    ) %>%
    mutate(
      `E[Y|t,c]` = case_when(c == "1" ~ low_con,
                             c == "2" ~ high_con,
                             TRUE ~ NaN),
      `sd[Y|t,c]` = case_when(c == "1" ~ sd_parameter * (high_con - low_con),
                              c == "2" ~  sd_parameter * (high_con - low_con),
                              TRUE ~ NaN),
      `P(Y|t,c)` = case_when(
        left_bound == right_bound ~ dnorm(x = left_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`),
        left_bound <= `E[Y|t,c]` ~ pnorm(right_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`) -
          pnorm(left_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`),
        TRUE ~ pnorm(left_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`, lower.tail = FALSE) -
          pnorm(right_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`, lower.tail = FALSE)
      )
    ) %>%
    mutate(`P(C=c|t)` = case_when(
      c == "2" ~ `P(2)`,
      c == "1" ~ `P(1)`
    ),
    `P(c,y|t)` = `P(C=c|t)` * `P(Y|t,c)`) %>%
    mutate(.by = obs_id,
           `P(Y=y|t)` = sum(`P(c,y|t)`)) %>%
    mutate(
      `P(C=c|y,t)` = `P(c,y|t)` / `P(Y=y|t)`) %>%
    return()
}
