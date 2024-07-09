#' Title
#'
#' @inheritParams simulate_mics
#' @inheritParams
#'
#' @param i Seed, used when running batches of simulations
#' @param n
#' @param t_dist
#' @param pi
#' @param `E[X|T,C]`
#' @param sd_vector
#' @param covariate_list
#' @param covariate_effect_vector
#' @param conc_limits_table
#' @param low_con
#' @param high_con
#' @param scale
#' @param model
#' @param approach
#' @param pi_formula
#' @param ncomp
#' @param pre_set_degrees
#' @param max_degree
#' @param degree_sets
#' @param nfolds
#' @param non_linear_term
#' @param covariates
#' @param fixed_side
#' @param extra_row
#' @param max_it
#' @param tol_ll
#' @param pi_link
#' @param verbose
#' @param model_coefficient_tolerance
#' @param maxiter_survreg
#' @param initial_weighting
#' @param sd_initial
#' @param reruns_allowed
#'
#' @return
#' @export
#'
#' @examples
simulation_run = function(i = 100,
                          n = 300,
                          t_dist = function(n) {
                            runif(n, min = 0, max = 16)
                          },
                          pi = function(t) {
                            z <- 0.17 + 0.025 * t - 0.00045 * t^2
                            tibble(`1` = 1 - z,
                                   `2` = z)
                          },
                          `E[X|T,C]` = function(t, c) {
                            case_when(c == "1" ~ -4 + (0.24 * t) - (0.0055 *t^2), c == "2" ~ 3 + 0.001 * t, TRUE ~ NaN)
                          },
                          sd_vector = c(`1` = 1, `2` = 1.05),
                          covariate_list = NULL,
                          covariate_effect_vector = c(0),
                          conc_limits_table = NULL,
                          low_con = -3,
                          high_con = 6,
                          scale = "log",
                          model = "pspline",
                          approach = "full",
                          pi_formula = c == "2" ~ s(t),
                          ncomp = 2,
                          pre_set_degrees = NULL,
                          max_degree = 8,
                          degree_sets = "matched",
                          nfolds = 10,
                          non_linear_term = "t",
                          covariates = NULL,
                          fixed_side = NULL,
                          extra_row = FALSE,
                          max_it = 3000,
                          tol_ll = 1e-06,
                          pi_link = "logit",
                          verbose = 3,
                          model_coefficient_tolerance = 1e-05,
                          maxiter_survreg = 30,
                          initial_weighting = 3,
                          sd_initial = 0.2,
                          reruns_allowed = 3
                          ){
  set.seed(i)
  simulated_data =
    simulate_mics(
      n = n,
      t_dist = t_dist,
      pi = pi,
      `E[X|T,C]` = `E[X|T,C]`,
      sd_vector = sd_vector,
      covariate_list = covariate_list,
      covariate_effect_vector = covariate_effect_vector,
      conc_limits_table = conc_limits_table,
      low_con = low_con,
      high_con = high_con,
      scale = scale
    )
  iteration_output = fit_EM_safe(
    model = model,
    approach = approach,
    pre_set_degrees = pre_set_degrees,
    max_degree = max_degree,
    degree_sets = degree_sets,
    visible_data = simulated_data,
    nfolds = nfolds,
    non_linear_term = non_linear_term,
    covariates = covariates,
    pi_formula = pi_formula,
    fixed_side = fixed_side,
    extra_row = extra_row,
    max_it = max_it,
    ncomp = ncomp,
    tol_ll = tol_ll,
    pi_link = pi_link,
    verbose = verbose,
    model_coefficient_tolerance = model_coefficient_tolerance,
    maxiter_survreg = maxiter_survreg,
    initial_weighting = initial_weighting,
    sd_initial = sd_initial,
    scale = scale,
    reruns_allowed = reruns_allowed
  )

  iteration_output$iteration_number = i
  return(iteration_output)
}

fit_EM_safe = safely(fit_EM)
