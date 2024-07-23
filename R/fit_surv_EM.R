#' Title
#'
#' @param pre_set_degrees
#' @param max_degree
#' @param degree_sets
#' @param visible_data
#' @param nfolds
#' @param non_linear_term
#' @param covariates
#' @param pi_formula
#' @param max_it
#' @param ncomp
#' @param tol_ll
#' @param pi_link
#' @param verbose
#' @param model_coefficient_tolerance
#' @param initial_weighting
#' @param sd_initial
#'
#' @return
#' @keywords internal
#'
#' @examples
fit_surv_EM = function(pre_set_degrees = NULL, #c(7,7)
                       max_degree,
                       degree_sets = "matched",
                       #"independent"
                       visible_data,
                       nfolds,
                       non_linear_term = "t",
                       covariates = NULL,
                       pi_formula = c == "2" ~ s(t),
                       max_it = 3000,
                       ncomp = 2,
                       tol_ll = 1e-6,
                       pi_link = "logit",
                       verbose = 3,
                       model_coefficient_tolerance = 0.00001,
                       initial_weighting = 9,
                       sd_initial = 0.2) {
  if (is.null(pre_set_degrees)) {
    cv_results = full_surv_cv(
      max_degree = max_degree,
      degree_sets = degree_sets,
      visible_data = visible_data,
      nfolds = nfolds,
      non_linear_term = non_linear_term,
      covariates = covariates,
      pi_formula = pi_formula,
      max_it = max_it,
      ncomp = ncomp,
      tol_ll = tol_ll,
      pi_link = pi_link,
      verbose = verbose,
      model_coefficient_tolerance = model_coefficient_tolerance,
      initial_weighting = initial_weighting,
      sd_initial = sd_initial
    ) %>%
      summarize(
        .by = c(degree_1, degree_2),
        log_likelihood = sum(fold_likelihood)
      ) %>%
      arrange(desc(log_likelihood))

    mu_formula = write_all_surv_formulas(non_linear_term,
                                         pull_top_degree_set(cv_results),
                                         covariates)
  } else{
    if(any(pre_set_degrees == 1)){
      errorCondition("degrees of freedom for pspline in surv package must be greater than 2")
    }
    mu_formula = write_all_surv_formulas(non_linear_term, pre_set_degrees, covariates)
    cv_results = NULL
  }
  output = EM_algorithm(
    visible_data = visible_data,
    model = "surv",
    #"mgcv"
    mu_formula = mu_formula,
    #mu_formula = yi ~ s(t),
    pi_formula = pi_formula,
    #or: c == "2" ~ lo(t)
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
    maxiter_survreg = 30,
    initial_weighting = initial_weighting,
    sd_initial = sd_initial,
    stop_on_likelihood_drop = FALSE,
    n_models = 100,
    seed = NULL,
    randomize = "all"
  )

  output$cv_results = cv_results

  output %>% return()

}



full_surv_cv = function(max_degree = 10,
                        degree_sets = "independent",
                        visible_data,
                        nfolds = 10,
                        non_linear_term = "t",
                        covariates = NULL,
                        pi_formula = c == "2" ~ s(t),
                        max_it = 3000,
                        ncomp = 2,
                        tol_ll = 1e-6,
                        pi_link = "logit",
                        verbose = 3,
                        model_coefficient_tolerance = 0.00001,
                        initial_weighting = 9,
                        sd_initial = 0.2,
                        scale = NULL){
  create_degree_combinations_surv(max_degree, ncomp, degree_sets) %>%
    map(
      .,
      ~ single_cv_surv(
        degrees = .x,
        visible_data = visible_data,
        nfolds = nfolds,
        non_linear_term = non_linear_term,
        covariates = covariates,
        pi_formula = pi_formula,
        max_it = max_it,
        ncomp = ncomp,
        tol_ll = tol_ll,
        pi_link = pi_link,
        verbose = verbose,
        model_coefficient_tolerance = model_coefficient_tolerance,
        initial_weighting = initial_weighting,
        sd_initial = sd_initial,
        scale = scale
      )
    ) %>% data.table::rbindlist() %>% tibble %>% return()

}




single_cv_surv = function(degrees,
                          visible_data,
                          nfolds,
                          non_linear_term = "t",
                          covariates = NULL,
                          pi_formula = c == "2" ~ s(t),
                          max_it = 3000,
                          ncomp = 2,
                          tol_ll = 1e-6,
                          pi_link = "logit",
                          verbose = 3,
                          model_coefficient_tolerance = 0.00001,
                          initial_weighting = 8,
                          sd_initial = 0.2,
                          scale = NULL) {
  message("CV for degrees", degrees)

  tibble(fold_likelihood = map_dbl(
    1:nfolds,
    ~ get_fold_likelihood_surv(
      .x,
      visible_data = assign_folds(visible_data, nfolds),
      degrees,
      non_linear_term = non_linear_term,
      covariates = covariates,
      pi_formula = pi_formula,
      max_it = max_it,
      ncomp = ncomp,
      tol_ll = tol_ll,
      pi_link = pi_link,
      verbose = verbose,
      model_coefficient_tolerance = model_coefficient_tolerance,
      initial_weighting = initial_weighting,
      sd_initial = sd_initial,
      scale = scale
    )
  )) %>%
    mutate(fold = row_number(),
           degree_1 = degrees[1],
           degree_2 = degrees[2]) %>%
    return()
}

get_fold_likelihood_surv = function(i,
                                    visible_data,
                                    degrees,
                                    non_linear_term = "t",
                                    covariates = NULL,
                                    pi_formula = c == "2" ~ s(t),
                                    max_it = 3000,
                                    ncomp = 2,
                                    tol_ll = 1e-6,
                                    pi_link = "logit",
                                    verbose = 3,
                                    model_coefficient_tolerance = 0.00001,
                                    initial_weighting = 8,
                                    sd_initial = 0.2,
                                    scale = NULL) {
  test = i

  mu_formula = write_all_surv_formulas(non_linear_term, degrees, covariates)

  ##add check for if fold column exists
  training_set = visible_data %>% filter(fold != test)
  testing_set = visible_data %>% filter(fold == test)

  trained_model = EM_algorithm(
    visible_data = training_set,
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
    maxiter_survreg = 30,
    initial_weighting = initial_weighting,
    sd_initial = sd_initial,
    stop_on_likelihood_drop = FALSE,
    n_models = 100,
    seed = NULL,
    randomize = "all",
    scale = scale
  )
  calculate_fold_likelihood(testing_set, trained_model$mu_model, trained_model$pi_model)

}


write_single_surv_formula = function(non_linear_term, degrees, covariates){
  reformulate(
    termlabels = c(
      paste0("pspline(", non_linear_term, ",", "df = ", degrees, ")"),
      covariates
    ),
    response = "Surv(time = left_bound, time2 = right_bound, type = 'interval2')"
  )
}

write_all_surv_formulas = function(non_linear_term, degrees, covariates){
  purrr::map(degrees, ~ write_single_surv_formula(non_linear_term, .x, covariates))
}

create_degree_combinations_surv = function(max_degree, ncomp, degree_sets = "matched") {
  if (ncomp == 2 & degree_sets == "independent") {
    deg_table = dplyr::cross_join(degree_tib_surv(1, max_degree),
                                  degree_tib_surv(2, max_degree))
  } else if (ncomp == 2 & degree_sets == "matched") {
    deg_table = tibble(degree_tib_surv(1, max_degree),
                       degree_tib_surv(2, max_degree))
  } else{
    deg_table = tibble(degree_tib_surv(1, max_degree))
  }
  purrr::transpose(deg_table) %>% map(., unlist) %>% map(., unname) %>% return()
}

degree_tib_surv = function(i, max_degree){
  a = tibble(placeholder = 2:max_degree)
  colnames(a) <- paste0("comp", i, "_degree")
  return(a)
}
