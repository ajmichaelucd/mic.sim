#' Title
#'
#' @param possible_data
#' @param mu_models
#' @param pi_model
#' @param fixed_side
#' @param extra_row
#'
#' @return
#' @export
#'
#' @examples
E_step_reduced = function(possible_data, mu_models, pi_model, fixed_side, extra_row){
  possible_data %<>% calculate_density_obs_reduced(., mu_models, fixed_side, extra_row) %>%
    pi_model_predictions(., pi_model) %>%
    calculate_new_weights() %>% return()
}



calculate_density_obs_reduced = function(possible_data, mu_models, fixed_side, extra_row){
  if(fixed_side == "RC" & !extra_row){
    possible_data %>%
      mutate(
        `E[Y|t,c]` = if_else(c == 1, predict(mu_models[[1]], newdata = possible_data), NA_real_),
        `sd[Y|t,c]` = if_else(c == 1, mu_models[[1]]$scale, NA_real_),
        `P(Y|t,c)` = case_when(
          c == 1 & left_bound == right_bound ~ dnorm(x = left_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`),
          c == 1 & left_bound <= `E[Y|t,c]` ~ pnorm(right_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`) -
            pnorm(left_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`),
          c == 1 ~ pnorm(left_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`, lower.tail = FALSE) -
            pnorm(right_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`, lower.tail = FALSE),
          TRUE ~ (right_bound == Inf) %>% as.numeric()
        )
      )
  }else if(fixed_side == "LC" & !extra_row){
    possible_data %>%
      mutate(
        `E[Y|t,c]` = if_else(c == 2, predict(mu_models[[1]], newdata = possible_data), NA_real_),
        `sd[Y|t,c]` = if_else(c == 2, mu_models[[1]]$scale, NA_real_),
        `P(Y|t,c)` = case_when(
          c == 2 & left_bound == right_bound ~ dnorm(x = left_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`),
          c == 2 & left_bound <= `E[Y|t,c]` ~ pnorm(right_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`) -
            pnorm(left_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`),
          c == 2 ~ pnorm(left_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`, lower.tail = FALSE) -
            pnorm(right_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`, lower.tail = FALSE),
          TRUE ~ (left_bound == -Inf ) %>% as.numeric()
        )
      )
  }else if(fixed_side == "RC" & extra_row){
    possible_data %>%
      mutate(
        `E[Y|t,c]` = if_else(c == 1, predict(mu_models[[1]], newdata = possible_data), NA_real_),
        `sd[Y|t,c]` = if_else(c == 1, mu_models[[1]]$scale, NA_real_),
        `P(Y|t,c)` = case_when(
          c == 1 & left_bound == right_bound ~ dnorm(x = left_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`),
          c == 1 & left_bound <= `E[Y|t,c]` ~ pnorm(right_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`) -
            pnorm(left_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`),
          c == 1 ~ pnorm(left_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`, lower.tail = FALSE) -
            pnorm(right_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`, lower.tail = FALSE),
          TRUE ~ (right_bound == Inf | right_bound == high_con) %>% as.numeric()
        )
      )
  }else if(fixed_side == "LC" & extra_row){
    possible_data %>%
      mutate(
        `E[Y|t,c]` = if_else(c == 2, predict(mu_models[[1]], newdata = possible_data), NA_real_),
        `sd[Y|t,c]` = if_else(c == 2, mu_models[[1]]$scale, NA_real_),
        `P(Y|t,c)` = case_when(
          c == 2 & left_bound == right_bound ~ dnorm(x = left_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`),
          c == 2 & left_bound <= `E[Y|t,c]` ~ pnorm(right_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`) -
            pnorm(left_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`),
          c == 2 ~ pnorm(left_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`, lower.tail = FALSE) -
            pnorm(right_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`, lower.tail = FALSE),
          TRUE ~ (left_bound == -Inf | left_bound == low_con) %>% as.numeric()
        )
      )
  }else{
    errorCondition("Invalid value for either fixed_side or extra_row")
  }

}

