#' Title
#'
#' @param testing_set
#' @param trained_mu_model
#' @param trained_pi_model
#' @param approach
#' @param fixed_side
#' @param extra_row
#' @param ecoff
#' @param ncomp
#' @param converge
#' @param max_out_break
#'
#' @return
#' @keywords internal
#'
#' @examples
calculate_fold_likelihood_all = function(testing_set,
                                         trained_mu_model,
                                         trained_pi_model,
                                         approach = "full",
                                         fixed_side = NULL,
                                         extra_row = FALSE,
                                         ecoff = NA,
                                         ncomp = 2,
                                         converge,
                                         max_out_break = FALSE) {
  if (converge == "NO") {
    return(NaN)
  } else{
    if (max_out_break && converge == "iterations") {
      return(NaN)
    } else{
      if (approach == "full" & ncomp > 1) {
        calculate_fold_likelihood.full(testing_set, trained_mu_model, trained_pi_model) %>% return()
      } else if (approach == "reduced" & !is.null(fixed_side)) {
        ECOFF = readr::parse_number(as.character(ecoff)) %>% log2()

        calculate_fold_likelihood.reduced(
          testing_set,
          trained_mu_model,
          trained_pi_model,
          fixed_side = fixed_side,
          extra_row = extra_row,
          ECOFF = ECOFF
        ) %>% return()
      } else if (ncomp == 1) {
        calculate_fold_likelihood.single_comp(testing_set,
                                              trained_mu_model)
      } else{
        errorCondition("If using reduced model, need to supply a value for fixed_side")
      }
    }
  }
}

calculate_fold_likelihood.full = function(testing_set, trained_mu_model, trained_pi_model){

  testing_set %>% add_obs_id() %>% reframe(.by = everything(),
                                           c = as.character(1:2)) %>%
    calculate_density_obs(., trained_mu_model) %>%
    pi_model_predictions(., trained_pi_model) %>%
    calculate_new_weights(.) %>%
    calculate_log_likelihood(.) %>% return()
}

calculate_fold_likelihood.reduced = function(testing_set, trained_mu_model, trained_pi_model, fixed_side, extra_row, ECOFF){
  testing_set %>% add_obs_id() %>% reframe(.by = everything(),
                                           c = as.character(1:2)) %>%
    calculate_density_obs_reduced(., trained_mu_model, fixed_side, extra_row, ECOFF) %>%
    pi_model_predictions(., trained_pi_model) %>%
    calculate_new_weights(.) %>%
    calculate_log_likelihood(.) %>% return()



}

calculate_fold_likelihood.single_comp = function(testing_set, trained_mu_model){
  testing_set %>% add_obs_id() %>% mutate(
    `E[Y|t]` = predict(trained_mu_model, newdata = testing_set),
    `sd[Y|t]` = get_scale(trained_mu_model),

    `P(Y|t)` = case_when(
      left_bound == right_bound ~ dnorm(x = left_bound, mean = `E[Y|t]`, sd =  `sd[Y|t]`),
      left_bound <= `E[Y|t]` ~ pnorm(right_bound, mean = `E[Y|t]`, sd =  `sd[Y|t]`) -
        pnorm(left_bound, mean = `E[Y|t]`, sd =  `sd[Y|t]`),
      TRUE ~ pnorm(left_bound, mean = `E[Y|t]`, sd =  `sd[Y|t]`, lower.tail = FALSE) -
        pnorm(right_bound, mean = `E[Y|t]`, sd =  `sd[Y|t]`, lower.tail = FALSE)
    ),
    log_likelihood_i = log(`P(Y|t)`)
  ) %>% summarise(log_like = sum(log_likelihood_i)) %>% pull(log_like) %>% return()

}


