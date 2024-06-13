
out = full_surv_cv(visible_data = visible_data)


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
                        initial_weighting = 8,
                        sd_initial = 0.2){
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
    sd_initial = sd_initial
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
                     sd_initial = 0.2) {
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
      sd_initial = sd_initial
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
                               sd_initial = 0.2) {
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
    randomize = "all"
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
