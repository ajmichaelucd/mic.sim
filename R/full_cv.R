#' Title
#'
#' @param model
#' @param approach
#' @param max_degree
#' @param degree_sets
#' @param visible_data
#' @param nfolds
#' @param non_linear_term
#' @param covariates
#' @param pi_formula
#' @param fixed_side
#' @param extra_row
#' @param max_it
#' @param ncomp
#' @param tol_ll
#' @param pi_link
#' @param verbose
#' @param model_coefficient_tolerance
#' @param maxiter_survreg
#' @param initial_weighting
#' @param sd_initial
#' @param scale
#'
#' @return
#' @keywords internal
#'
#' @examples
full_cv = function(
    model = "surv",
    approach = "full",
    max_degree = 10,
    degree_sets = "independent",
    visible_data,
    nfolds = 10,
    non_linear_term = "t",
    covariates = NULL,
    pi_formula = c == "2" ~ s(t),
    fixed_side = NULL,
    extra_row = FALSE,
    max_it = 3000,
    ncomp = 2,
    tol_ll = 1e-6,
    pi_link = "logit",
    verbose = 3,
    model_coefficient_tolerance = 0.00001,
    maxiter_survreg = 30,
    initial_weighting = 3,
    sd_initial = 0.2,
    scale = NULL,
    reruns_allowed = 3){
  create_degree_combinations_all(max_degree, ncomp, degree_sets, model, approach) %>%
    map(
      .,
      ~ single_cv_all(
        model = model,
        approach = approach,
        degrees = .x,
        visible_data = visible_data,
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
    ) %>% data.table::rbindlist() %>% tibble %>% return()

}
