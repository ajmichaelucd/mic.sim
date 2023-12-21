#' Title
#'
#' @param random_seeds_vector
#' @param visible_data
#' @param mu_formula
#' @param pi_formula
#' @param censored_side
#' @param extra_row
#' @param max_it
#' @param ncomp
#' @param tol_ll
#' @param pi_link
#' @param verbose
#' @param model_coefficient_tolerance
#' @param maxiter_survreg
#' @param sd_initial
#' @param randomize
#'
#' @return
#' @export
#'
#' @examples
EM_fms_surv_batch_run =
  function(random_seeds_vector,
           visible_data,
           mu_formula = Surv(time = left_bound,
                             time2 = right_bound,
                             type = "interval2") ~ pspline(t, df = 0, caic = TRUE),
           pi_formula = c == "2" ~ s(t),
           censored_side = "RC",
           extra_row = FALSE,
           max_it = 500,
           ncomp = 2,
           tol_ll = 1e-6,
           pi_link = "logit",
           verbose = 1,
           model_coefficient_tolerance = 0.00001,
           maxiter_survreg = 30,
           sd_initial = 0.2,
           randomize = "all") {
    purrr::map2(
      random_seeds_vector,
      1:length(random_seeds_vector),
      ~EM_fms_surv_single_run(
        .x,
        .y,
        visible_data,
        mu_formula,
        pi_formula,
        censored_side,
        extra_row,
        max_it,
        ncomp,
        tol_ll,
        pi_link,
        verbose,
        model_coefficient_tolerance,
        maxiter_survreg,
        sd_initial,
        randomize,
        n_models = length(random_seeds_vector)
      ),
      progress = TRUE
    )
  }



EM_fms_surv_single_run = function(seed,
                                 iter,
                                 visible_data,
                                 mu_formula,
                                 pi_formula,
                                 censored_side,
                                 extra_row,
                                 max_it,
                                 ncomp,
                                 tol_ll,
                                 pi_link,
                                 verbose,
                                 model_coefficient_tolerance,
                                 maxiter_survreg,
                                 sd_initial,
                                 randomize,
                                 n_models) {
  EM = purrr::possibly(.f = EM_algorithm_safety_surv, otherwise = "Error")
  message("starting iteration ", iter)
  single_model_output <- visible_data %>%
    EM(
      visible_data = visible_data,
      mu_formula = mu_formula,
      pi_formula = pi_formula,
      censored_side = censored_side,
      extra_row = extra_row,
      max_it = max_it,
      ncomp = ncomp,
      tol_ll = tol_ll,
      pi_link = pi_link,
      verbose = verbose,
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
    final_like = single_model_output$likelihood %>% select(step, likelihood) %>% tail(1) %>% mutate(iter = iter,
                                                                                                    seed = seed,
                                                                                                    converge = single_model_output$converge)
  } else{
    final_like = tibble(
      step = NA_integer_,
      likelihood = NA_integer_,
      iter = iter,
      seed = seed,
      converge = "Error"
    )
  }
  list(
    output = single_model_output,
    seed = seed,
    iter = iter,
    final_like = final_like
  )

}
