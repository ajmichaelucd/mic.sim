#' Title
#'
#' @param seed
#' @param iter
#' @param visible_data
#' @param mu_formula
#' @param pi_formula
#' @param max_it
#' @param ncomp
#' @param tol_ll
#' @param pi_link
#' @param verbose
#' @param initial_weighting
#' @param sd_initial
#'
#' @return
#' @export
#'
#' @examples
EM_fm_surv_single_run = function(seed,
                                 iter,
                                 visible_data,
                                 mu_formula,
                                 pi_formula,
                                 max_it,
                                 ncomp,
                                 tol_ll,
                                 pi_link,
                                 verbose,
                                 initial_weighting,
                                 model_coefficient_tolerance,
                                 maxiter_survreg,
                                 sd_initial,
                                 randomize,
                                 n_models) {
  EM = purrr::possibly(.f = EM_algorithm, otherwise = "Error")
message("starting iteration ", iter)
  single_model_output <- visible_data %>%
    EM(
      visible_data = .,
      model = "surv",
      mu_formula = mu_formula,
      pi_formula = pi_formula,
      max_it = max_it,
      ncomp = ncomp,
      tol_ll = tol_ll,
      pi_link = pi_link,
      verbose = verbose,
      initial_weighting = initial_weighting,
      browse_at_end = FALSE,
      browse_each_step = FALSE,
      plot_visuals = FALSE,
      stop_on_likelihood_drop = FALSE,
      pause_on_likelihood_drop = FALSE,
      model_coefficient_tolerance = model_coefficient_tolerance,
      maxiter_survreg = maxiter_survreg,
      sd_initial = sd_initial,
      seed = seed,
      randomize = randomize,
      n_models = n_models
    )

final_like = parse_surv_single_run_output(single_model_output, iter, seed)

  list(
    output = single_model_output,
    seed = seed,
    iter = iter,
    final_like = final_like
  ) %>% return()

}

check_surv_single_run_output_mu_model = function(mu_model){
  !is.na(mu_model$scale) & !is.na(predict(mu_model, newdata = data.frame(t = 1))) %>% unname
}



parse_surv_single_run_output = function(single_model_output, iter, seed){
  if (length(single_model_output) > 1) {

    final_like = single_model_output$likelihood %>%
      select(step, loglikelihood) %>%
      tail(1) %>%
      mutate(iter = iter,
             seed = seed,
             converge = single_model_output$converge)
    if(single_model_output$ncomp > 1){
      final_like = final_like %>% mutate(
        comp_conv = case_when(
          (check_surv_single_run_output_mu_model(single_model_output$mu_model[[1]]) & check_surv_single_run_output_mu_model(single_model_output$mu_model[[2]])) ~ "both",
          check_surv_single_run_output_mu_model(single_model_output$mu_model[[1]]) ~ "comp 1",
          check_surv_single_run_output_mu_model(single_model_output$mu_model[[2]]) ~ "comp 2",
          TRUE ~ "neither"
        ))
    }else{
      final_like = final_like %>% mutate(
        comp_conv = "yes")
    }
  } else{
    final_like = tibble(
      step = NA_integer_,
      loglikelihood = NA_integer_,
      iter = iter,
      seed = seed,
      converge = "Error",
      comp_conv = "neither"
    )
  }
  return(final_like)
}
