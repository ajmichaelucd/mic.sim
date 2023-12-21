#' Title
#'
#' @param random_seeds_vector
#' @param visible_data
#' @param mu_formula
#' @param pi_formula
#' @param max_it
#' @param ncomp
#' @param tol_ll
#' @param pi_link
#' @param verbose
#' @param initial_weighting
#' @param model_coefficient_tolerance
#' @param maxiter_survreg
#' @param sd_initial
#' @param randomize
#'
#' @return
#' @export
#'
#' @examples
EM_fm_surv_batch_run =
  function(random_seeds_vector,
           visible_data,
           mu_formula = Surv(time = left_bound,
                             time2 = right_bound,
                             type = "interval2") ~ pspline(t, df = 0, caic = TRUE),
           pi_formula = c == "2" ~ s(t),
           max_it = 500,
           ncomp = 2,
           tol_ll = 1e-6,
           pi_link = "logit",
           verbose = 1,
           initial_weighting = 7,
           model_coefficient_tolerance = 0.00001,
           maxiter_survreg = 30,
           sd_initial = 0.2,
           randomize = "all") {
    purrr::map2(
      random_seeds_vector,
      1:length(random_seeds_vector),
      ~EM_fm_surv_single_run(
        .x,
        .y,
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
        n_models = length(random_seeds_vector)
      ),
      progress = TRUE
    )
  }
