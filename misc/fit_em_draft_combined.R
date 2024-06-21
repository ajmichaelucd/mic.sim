fit_EM = function(model = "surv", #"polynomial",
                       pre_set_degrees = NULL, #c(7,7)
                       max_degree = 8,
                       degree_sets = "matched",
                       #"independent"
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
                       sd_initial = 0.2) {

  if(model == "surv"){
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
  }else if(model == "polynomial"){
    if (is.null(pre_set_degrees)) {
      cv_results = full_polynomial_cv(
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

      mu_formula = write_all_polynomial_formulas(non_linear_term,
                                                 pull_top_degree_set(cv_results),
                                                 covariates)
    } else{
      mu_formula = write_all_polynomial_formulas(non_linear_term, pre_set_degrees, covariates)
      cv_results = NULL
    }
  }else{
    errorCondition("Currently the two options for fitting the EM algorithm are 'surv' and 'polynomial'")
  }

  output = EM_algorithm(
    visible_data = visible_data,
    model = model,
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
