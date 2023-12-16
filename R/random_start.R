#' Title
#'
#' @param visible_data
#' @param ncomp
#' @param sd_parameter
#'
#' @return
#' @export
#'
#' @examples
random_start = function(visible_data, ncomp, sd_parameter = 0.2, n_models){

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

  `P(1)` = rbeta(n = 1, shape1 = (lc + (0.5 * int) + 1), shape2 = (rc + (0.5 * int) + 1))
  `P(2)` = 1 - `P(1)`

  mu_sd = (sd_parameter * (high_con - low_con)) / n_models

  visible_data %>%
    reframe(.by = everything(),
            c = as.character(1:2)
    ) %>%
    mutate(
      `E[Y|t,c]` = case_when(c == "1" ~ low_con + rnorm(1, 0, mu_sd),
                             c == "2" ~ high_con + rnorm(1, 0, mu_sd),
                             TRUE ~ NaN),
      `sd[Y|t,c]` = case_when(c == "1" ~ exp(log(sd_parameter * (high_con - low_con)) + rnorm(10, 0, 0.25)),
                              c == "2" ~  exp(log(sd_parameter * (high_con - low_con)) + rnorm(10, 0, 0.25)),
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
