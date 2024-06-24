#' Title
#'
#' @param testing_set
#' @param trained_mu_model
#' @param trained_pi_model
#' @param approach
#' @param fixed_side
#' @param extra_row
#'
#' @return
#' @export
#'
#' @examples
calculate_fold_likelihood_all = function(testing_set,
                                         trained_mu_model,
                                         trained_pi_model,
                                         approach = "full",
                                         fixed_side = NULL,
                                         extra_row = FALSE) {
  if (approach == "full") {
    calculate_fold_likelihood.full(testing_set, trained_mu_model, trained_pi_model) %>% return()
  } else if (approach == "reduced" & !is.null(fixed_side)) {
    calculate_fold_likelihood.reduced(testing_set,
                                      trained_mu_model,
                                      trained_pi_model,
                                      fixed_side,
                                      extra_row) %>% return()
  } else{
    errorCondition("If using reduced model, need to supply a value for fixed_side")
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

calculate_fold_likelihood.reduced = function(testing_set, trained_mu_model, trained_pi_model, fixed_side, extra_row){
  testing_set %>% add_obs_id() %>% reframe(.by = everything(),
                                           c = as.character(1:2)) %>%
    calculate_density_obs_reduced(., trained_mu_model, fixed_side, extra_row) %>%
    pi_model_predictions(., trained_pi_model) %>%
    calculate_new_weights(.) %>%
    calculate_log_likelihood(.) %>% return()



}


