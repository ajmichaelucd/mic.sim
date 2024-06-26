#' Title
#'
#' @param model
#' @param approach
#' @param pre_set_degrees
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
#'
#' @return
#' @export
#'
#' @examples
fit_EM = function(model = "surv", #"polynomial",
                  approach = "full", #"reduced"
                  pre_set_degrees = NULL, #c(7,7)
                  max_degree = 8,
                  degree_sets = "matched",
                  #"independent"
                  visible_data,
                  nfolds = 10,
                  non_linear_term = "t",
                  covariates = NULL,
                  pi_formula = c == "2" ~ s(t),
                  fixed_side = NULL,
                  extra_row = FALSE,
                  max_it = 3000,
                  ncomp = 2, #relevant
                  tol_ll = 1e-6,
                  pi_link = "logit",
                  verbose = 3,
                  model_coefficient_tolerance = 0.00001,
                  maxiter_survreg = 30,
                  initial_weighting = 9,
                  sd_initial = 0.2,
                  scale = NULL,
                  reruns_allowed = 3) {
  ##check here if approach is reduced but fixed side is null then we have a problem
  if (is.null(pre_set_degrees)) {
    cv_results_intermediate = full_cv(
      model = model,
      approach = approach,
      max_degree = max_degree,
      degree_sets = degree_sets,
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


    cv_results =
      cv_results_intermediate %>%
      summarize(
        .by = starts_with("degree"),
        log_likelihood = sum(fold_likelihood),
        total_repeats = sum(repeats)
      ) %>%
      arrange(desc(log_likelihood))



    mu_formula = write_all_formulas(non_linear_term,
                                    pull_top_degree_set(cv_results),
                                    covariates, model)
  } else{
    if(model == "surv" & any(pre_set_degrees == 1)){
      errorCondition("degrees of freedom for pspline in surv package must be at least 2")
    }
    mu_formula = write_all_formulas(non_linear_term, pre_set_degrees, covariates, model)
    cv_results = NULL
  }
  ##add a choice here between full and reduced models FIX

  if(approach == "full"){
    output = EM_algorithm(
      visible_data = visible_data,
      model = model,
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
      n_models = 100,
      seed = NULL,
      randomize = "all",
      scale = scale
    )
  }else if(approach == "reduced" & !is.null(fixed_side)){
    output = EM_algorithm_reduced(fixed_side = fixed_side,
                                  extra_row = extra_row,
                                  visible_data = visible_data,
                                  model = model,
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
                                  covariates = covariates,
                                  scale = scale
    )
  }else{
    errorCondition("Values for approach are 'full' and 'reduced', if using reduced model, supply a value for fixed_side (RC or LC) and consider extra_row")
  }

  output$cv_results = cv_results

  output %>% return()

}




pull_top_degree_set = function(cv_results) {
  cv_results %>% select(-c(log_likelihood,total_repeats)) %>% purrr::map_dbl(., head(1)) %>% unname %>% return()
}
