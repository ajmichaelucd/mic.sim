#' Title
#'
#' @param visible_data
#' @param censored_side
#' @param extra_row
#' @param randomize
#' @param sd_parameter
#' @param n_models
#'
#' @return
#' @export
#'
#' @examples
random_start_safety = function(visible_data, censored_side, extra_row, randomize, sd_parameter, n_models){
  low_con = visible_data$low_con %>% unique
  high_con = visible_data$high_con %>% unique
  if(censored_side == "RC" & !extra_row){
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

    if(randomize %in% c("all", "mu")){
      mu_sd = (sd_parameter * (high_con - low_con)) / sqrt(n_models)
      mu_est = low_con +  rnorm(1, 0, mu_sd)
    }else{
      mu_est = low_con
    }
    if(randomize %in% c("all", "sigma")){
      scale_est = exp(log(sd_parameter * (high_con - low_con)) + rnorm(1, 0, 0.25))
    }else{
      scale_est = sd_parameter * (high_con - low_con)
    }
    if(randomize %in% c("all", "pi")){
      `P(1)` = rbeta(n = 1, shape1 = (lc + int + 1), shape2 = (rc + 1))
      `P(2)` = 1 - `P(1)`
    }else{
      `P(1)` = (lc + int + 1) / (n_obs + 2)
      `P(2)` = (rc + 1) / (n_obs + 2)
    }

    possible_data =
      visible_data %>%
      reframe(.by = everything(),
              c = as.character(1:2)
      ) %>%
      mutate(
        `E[Y|t,c]` = if_else(c == 1, mu_est, NA_real_),
        `sd[Y|t,c]` = if_else(c == 1, scale_est, NA_real_),
        sigma_initial = `sd[Y|t,c]`,
        `P(Y|t,c)` = case_when(
          c == 1 & left_bound == right_bound ~ dnorm(x = left_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`),
          c == 1 & left_bound <= `E[Y|t,c]` ~ pnorm(right_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`) -
            pnorm(left_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`),
          c == 1 ~ pnorm(left_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`, lower.tail = FALSE) -
            pnorm(right_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`, lower.tail = FALSE),
          TRUE ~ (right_bound == Inf) %>% as.numeric()
        ),
        `P(C=c|t)` = case_when(
          c == "2" ~ `P(2)`,
          c == "1" ~ `P(1)`
        ),
        `P(c,y|t)` = `P(C=c|t)` * `P(Y|t,c)`
      ) %>%
      mutate(.by = obs_id,
             `P(Y=y|t)` = sum(`P(c,y|t)`)) %>%
      mutate(
        `P(C=c|y,t)` = `P(c,y|t)` / `P(Y=y|t)`)
  } else if(censored_side == "LC" & !extra_row){ #%>%
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

    if(randomize %in% c("all", "mu")){
      mu_sd = (sd_parameter * (high_con - low_con)) / n_models
      mu_est = high_con +  rnorm(1, 0, mu_sd)
    }else{
      mu_est = high_con
    }
    if(randomize %in% c("all", "sigma")){
      scale_est = exp(log(sd_parameter * (high_con - low_con)) + rnorm(1, 0, 0.25))
    }else{
      scale_est = sd_parameter * (high_con - low_con)
    }
    if(randomize %in% c("all", "pi")){
      `P(1)` = rbeta(n = 1, shape1 = (lc + 1), shape2 = (rc + int + 1))
      `P(2)` = 1 - `P(1)`
    }else{
      `P(1)` = (lc + 1) / (n_obs + 2)
      `P(2)` = (rc + int + 1) / (n_obs + 2)
    }

    possible_data =
      visible_data %>%
      reframe(.by = everything(),
              c = as.character(1:2)
      ) %>%
      mutate(
        `E[Y|t,c]` = if_else(c == 2, mu_est, NA_real_),
        `sd[Y|t,c]` = if_else(c == 2, scale_est, NA_real_),
        sigma_initial = `sd[Y|t,c]`,
        `P(Y|t,c)` = case_when(
          c == 2 & left_bound == right_bound ~ dnorm(x = left_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`),
          c == 2 & left_bound <= `E[Y|t,c]` ~ pnorm(right_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`) -
            pnorm(left_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`),
          c == 2 ~ pnorm(left_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`, lower.tail = FALSE) -
            pnorm(right_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`, lower.tail = FALSE),
          TRUE ~ (left_bound == -Inf ) %>% as.numeric()
        ),
        `P(C=c|t)` = case_when(
          c == "2" ~ `P(2)`,
          c == "1" ~ `P(1)`
        ),
        `P(c,y|t)` = `P(C=c|t)` * `P(Y|t,c)`
      ) %>%
      mutate(.by = obs_id,
             `P(Y=y|t)` = sum(`P(c,y|t)`)) %>%
      mutate(
        `P(C=c|y,t)` = `P(c,y|t)` / `P(Y=y|t)`)
    }else if(censored_side == "LC" & extra_row){ #%>%
      visible_data <- visible_data %>% mutate(cens = case_when(
        left_bound == -Inf | right_bound == low_con | left_bound == low_con ~ "lc",
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

      if(randomize %in% c("all", "mu")){
        mu_sd = (sd_parameter * (high_con - (low_con + 1))) / n_models
        mu_est = high_con +  rnorm(1, 0, mu_sd)
      }else{
        mu_est = high_con
      }
      if(randomize %in% c("all", "sigma")){
        scale_est = exp(log(sd_parameter * (high_con - (low_con + 1))) + rnorm(1, 0, 0.25))
      }else{
        scale_est = sd_parameter * (high_con - (low_con + 1))
      }
      if(randomize %in% c("all", "pi")){
        `P(1)` = rbeta(n = 1, shape1 = (lc + 1), shape2 = (rc + int + 1))
        `P(2)` = 1 - `P(1)`
      }else{
        `P(1)` = (lc + 1) / (n_obs + 2)
        `P(2)` = (rc + int + 1) / (n_obs + 2)
      }

      possible_data =
        visible_data %>%
        reframe(.by = everything(),
                c = as.character(1:2)
        ) %>%
        mutate(
          `E[Y|t,c]` = if_else(c == 2, mu_est, NA_real_),
          `sd[Y|t,c]` = if_else(c == 2, scale_est, NA_real_),
          sigma_initial = `sd[Y|t,c]`,
          `P(Y|t,c)` = case_when(
            c == 2 & left_bound == right_bound ~ dnorm(x = left_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`),
            c == 2 & left_bound <= `E[Y|t,c]` ~ pnorm(right_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`) -
              pnorm(left_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`),
            c == 2 ~ pnorm(left_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`, lower.tail = FALSE) -
              pnorm(right_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`, lower.tail = FALSE),
            TRUE ~ (left_bound == -Inf | left_bound == low_con) %>% as.numeric()
          ),
          `P(C=c|t)` = case_when(
            c == "2" ~ `P(2)`,
            c == "1" ~ `P(1)`
          ),
          `P(c,y|t)` = `P(C=c|t)` * `P(Y|t,c)`
        ) %>%
        mutate(.by = obs_id,
               `P(Y=y|t)` = sum(`P(c,y|t)`)) %>%
        mutate(
          `P(C=c|y,t)` = `P(c,y|t)` / `P(Y=y|t)`)
      }else if(censored_side == "RC" & extra_row){
        visible_data <- visible_data %>% mutate(cens = case_when(
          left_bound == -Inf | right_bound == low_con ~ "lc",
          right_bound == Inf |
            left_bound == high_con | right_bound == high_con ~ "rc",
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

        if(randomize %in% c("all", "mu")){
          mu_sd = (sd_parameter * ((high_con - 1) - low_con)) / n_models
          mu_est = low_con +  rnorm(1, 0, mu_sd)
        }else{
          mu_est = low_con
        }
        if(randomize %in% c("all", "sigma")){
          scale_est = exp(log(sd_parameter * ((high_con - 1) - low_con)) + rnorm(1, 0, 0.25))
        }else{
          scale_est = sd_parameter * ((high_con - 1) - low_con)
        }
        if(randomize %in% c("all", "pi")){
          `P(1)` = rbeta(n = 1, shape1 = (lc + int + 1), shape2 = (rc + 1))
          `P(2)` = 1 - `P(1)`
        }else{
          `P(1)` = (lc + int + 1) / (n_obs + 2)
          `P(2)` = (rc + 1) / (n_obs + 2)
        }

        possible_data =
          visible_data %>%
          reframe(.by = everything(),
                  c = as.character(1:2)
          ) %>%
          mutate(
            `E[Y|t,c]` = if_else(c == 1, mu_est, NA_real_),
            `sd[Y|t,c]` = if_else(c == 1, scale_est, NA_real_),
            sigma_initial = `sd[Y|t,c]`,
            `P(Y|t,c)` = case_when(
              c == 1 & left_bound == right_bound ~ dnorm(x = left_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`),
              c == 1 & left_bound <= `E[Y|t,c]` ~ pnorm(right_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`) -
                pnorm(left_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`),
              c == 1 ~ pnorm(left_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`, lower.tail = FALSE) -
                pnorm(right_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`, lower.tail = FALSE),
              TRUE ~ (right_bound == Inf | right_bound == high_con) %>% as.numeric()
            ),
            `P(C=c|t)` = case_when(
              c == "2" ~ `P(2)`,
              c == "1" ~ `P(1)`
            ),
            `P(c,y|t)` = `P(C=c|t)` * `P(Y|t,c)`
          ) %>%
          mutate(.by = obs_id,
                 `P(Y=y|t)` = sum(`P(c,y|t)`)) %>%
          mutate(
            `P(C=c|y,t)` = `P(c,y|t)` / `P(Y=y|t)`)
        }else{
          TRUE ~ errorCondition("censored_side must be 'RC' or 'LC' and extra_row must be logical")
        }
  possible_data %>% return()
}
