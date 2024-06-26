#' Title
#'
#' @param visible_data
#' @param non_linear_term
#' @param covariates
#' @param pi_formula
#' @param max_it
#' @param ncomp
#' @param tol_ll
#' @param pi_link
#' @param verbose
#' @param model_coefficient_tolerance
#' @param sd_initial
#'
#' @return
#' @export
#'
#' @examples
initial_weighting_fit_linear_model = function(visible_data, non_linear_term, covariates, pi_formula, max_it, ncomp, tol_ll, pi_link, verbose, model_coefficient_tolerance, sd_initial){
  visible_data %>%
    EM_algorithm(
      model = "polynomial",
      mu_formula = write_all_formulas(non_linear_term = non_linear_term, degrees = rep(1, ncomp), covariates = covariates, model = "polynomial"),
      visible_data = .,
      non_linear_term = non_linear_term,
      covariates = covariates,
      pi_formula = pi_formula,
      max_it = max_it,
      ncomp = ncomp,
      tol_ll = tol_ll,
      pause_on_likelihood_drop = FALSE,
      pi_link = pi_link,
      verbose = verbose,
      model_coefficient_tolerance = model_coefficient_tolerance,
      initial_weighting = 8,
      sd_initial = sd_initial
    ) %>% return()
}
