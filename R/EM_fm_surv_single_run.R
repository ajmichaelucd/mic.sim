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
  EM = purrr::possibly(.f = EM_algorithm_surv, otherwise = "Error")
message("starting iteration ", iter)
  single_model_output <- visible_data %>%
    EM(
      visible_data = visible_data,
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

  if (length(single_model_output) > 1) {

    final_like = single_model_output$likelihood %>%
      select(step, likelihood) %>%
      tail(1) %>%
      mutate(iter = iter,
             seed = seed,
             converge = single_model_output$converge)
    if(ncomp > 1){
      final_like = final_like %>% mutate(
        comp_conv = case_when(
          (!is.na(single_model_output$mu_model[[1]]$scale)) & (!is.na(single_model_output$mu_model[[2]]$scale)) & !is.na(predict(single_model_output$mu_model[[1]], newdata = data.frame(t = 1))) & !is.na(predict(single_model_output$mu_model[[2]], newdata = data.frame(t = 1))) ~ "both",
          !is.na(single_model_output$mu_model[[1]]$scale) & !is.na(predict(single_model_output$mu_model[[1]], newdata = data.frame(t = 1))) ~ "comp 1",
          !is.na(single_model_output$mu_model[[2]]$scale) & !is.na(predict(single_model_output$mu_model[[2]], newdata = data.frame(t = 1))) ~ "comp 2",
          TRUE ~ "neither"
        ))
    }else{
      final_like = final_like %>% mutate(
        comp_conv = "yes")
    }
  } else{
    final_like = tibble(
      step = NA_integer_,
      likelihood = NA_integer_,
      iter = iter,
      seed = seed,
      converge = "Error",
      comp_conv = "neither"
    )
  }
  list(
    output = single_model_output,
    seed = seed,
    iter = iter,
    final_like = final_like
  )

}
