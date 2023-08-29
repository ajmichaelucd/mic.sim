
n_obs <- nrow(visible_data)

cens_counts <-
  visible_data %>%
  summarize(.by = cens,
            n = n()
            ) %>%
  pivot_wider(
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
        `sd[Y|t,c]` = case_when(c == "1" ~ 0.5*(high_con - low_con),
                                c == "2" ~ 0.5*(high_con - low_con),
                                TRUE ~ NaN),
        `P(Y|t,c)` =  if_else(
          left_bound == right_bound,
          dnorm(x = left_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`),
          pnorm(right_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`) -
            pnorm(left_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`)
        )
      ) %>%
      group_by(obs_id) %>%
      mutate(`P(C=c|t)` = case_when(
        c == "2" ~ `P(2)`, ########UNSURE ABOUT THIS SECTION
        c == "1" ~ `P(1)` ########UNSURE ABOUT THIS SECTION
      )) %>%  ########UNSURE ABOUT THIS SECTION
      mutate(`P(c,y|t)` = `P(C=c|t)` * `P(Y|t,c)`, ########UNSURE ABOUT THIS SECTION
             `P(Y=y|t)` = sum(`P(c,y|t)`), ########UNSURE ABOUT THIS SECTION
             `P(C=c|y,t)` = `P(c,y|t)` / `P(Y=y|t)`) %>%  ########UNSURE ABOUT THIS SECTION
      ungroup()
