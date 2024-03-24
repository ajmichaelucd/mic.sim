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
fit_surv_EM =
  function(
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
    initial_weighting = 9,
    model_coefficient_tolerance = 0.00001,
    maxiter_survreg = 30,
    sd_initial = 0.2,
    non_linear_term = "t",
    covariates = NULL) {
    EM_algorithm(
      visible_data = visible_data,
      model = "surv",
      mu_formula = mu_formula,
      pi_formula = pi_formula,
      max_it = max_it,
      ncomp = ncomp,
      tol_ll = tol_ll,
      browse_at_end = FALSE,
      browse_each_step = FALSE,
      plot_visuals = FALSE,
      prior_step_plot = FALSE,
      pause_on_likelihood_drop = FALSE,
      pi_link = pi_link,
      verbose = verbose,
      model_coefficient_tolerance = model_coefficient_tolerance,
      maxiter_survreg = maxiter_survreg,
      initial_weighting = initial_weighting,
      sd_initial = sd_initial,
      stop_on_likelihood_drop = FALSE,
      non_linear_term = non_linear_term,
      covariates = covariates
    ) %>% return()
    ##save linear model in step 9
  }
