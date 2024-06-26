#' Title
#'
#' @param model
#' @param approach
#' @param degrees
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
#' @param reruns_allowed
#'
#' @return
#' @export
#'
#' @examples
single_cv_all = function(model = "surv",
                         approach = "full",
                         degrees,
                         visible_data,
                         nfolds,
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
                         initial_weighting = 9,
                         sd_initial = 0.2,
                         scale = NULL,
                         reruns_allowed = 3) {
  for(i in 1:reruns_allowed){
    message("CV for degrees", degrees, "; attempt", i)

    fold_output = tibble(fold_likelihood = map_dbl(
      1:nfolds,
      ~ get_fold_likelihood_all(#_safe_single_output(
        model = model,
        approach = approach,
        .x,
        visible_data = assign_folds(visible_data, nfolds),
        degrees,
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
        scale = scale
      )
    ))

    rep = i - 1

    if((fold_output %>% filter(is.nan(fold_likelihood)) %>% nrow()) == 0){
      break
    }

  }

  tibble(fold_output, add_all_degrees_to_fold_likelihood(fold_output, degrees, nfolds)) %>%
    mutate(repeats = rep) %>%
    return()
}


add_single_degree_to_fold_likelihood = function(df, i, j, nfolds){
  tibble(!!paste0("degree_", i) := rep(j,nfolds))
}

add_all_degrees_to_fold_likelihood = function(df, degrees, nfolds){
  map2_dfc(1:length(degrees), degrees, ~add_single_degree_to_fold_likelihood(df, .x, .y, nfolds))
}
