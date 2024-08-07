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
                  sd_initial = 0.2) {
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
      sd_initial = sd_initial
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
    randomize = "all"
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
                         covariates = covariates
    )
  }else{
    errorCondition("Values for approach are 'full' and 'reduced', if using reduced model, supply a value for fixed_side (RC or LC) and consider extra_row")
  }

  output$cv_results = cv_results

  output %>% return()

}



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
                        initial_weighting = 9,
                        sd_initial = 0.2,
                        scale = NULL){
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
        scale = scale
      )
    ) %>% data.table::rbindlist() %>% tibble %>% return()

}


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
      ~ get_fold_likelihood_all_safe_single_output(
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


get_fold_likelihood_all_safe = safely(get_fold_likelihood_all, otherwise = NaN)


get_fold_likelihood_all_safe_single_output =
  function(model = "surv",
           approach = "full",
           i,
           visible_data,
           degrees,
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
           scale = NULL){

    out = get_fold_likelihood_all_safe(
      model = model,
      approach = approach,
      i = i,
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

    out$result %>% return()
  }





get_fold_likelihood_all = function(model = "surv",
                                    approach = "full",
                                    i,
                                    visible_data,
                                    degrees,
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
                                    scale = NULL) {
  test = i

  mu_formula = write_all_formulas(non_linear_term, degrees, covariates, model)

  ##add check for if fold column exists
  training_set = visible_data %>% filter(fold != test)
  testing_set = visible_data %>% filter(fold == test)


  ##OPTION FOR FULL OR REDUCED MODEL HERE
  if(approach == "full"){
  trained_model = EM_algorithm(
    visible_data = training_set,
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
    non_linear_term = non_linear_term,
    covariates = covariates,
    scale = scale
  )
  }else if(approach == "reduced"){
    trained_model = EM_algorithm_reduced(fixed_side = fixed_side,
                         extra_row = extra_row,
                         visible_data = training_set,
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
                         covariates = covariates
                         )
  }else{
    errorCondition("Use 'full' or 'reduced' for approach")
  }

  calculate_fold_likelihood_all(testing_set, trained_mu_model = trained_model$mu_model, trained_pi_model = trained_model$pi_model, approach = approach, fixed_side = fixed_side, extra_row = extra_row) %>%
    return()  ###MAY NEED TO FIX THIS TOO

  }


write_single_formula.surv = function(non_linear_term, degrees, covariates){
  reformulate(
    termlabels = c(
      paste0("pspline(", non_linear_term, ",", "df = ", degrees, ")"),
      covariates
    ),
    response = "Surv(time = left_bound, time2 = right_bound, type = 'interval2')"
  )
}

write_single_formula.polynomial = function(non_linear_term, degrees, covariates){
  reformulate(
    termlabels = c(
      paste0("poly(", non_linear_term, ",", "degree = ", degrees, ")"),
      covariates
    ),
    response = "Surv(time = left_bound, time2 = right_bound, type = 'interval2')"
  )
}

write_all_formulas = function(non_linear_term, degrees, covariates, model){
  if(model == "polynomial"){
    purrr::map(degrees, ~ write_single_formula.polynomial(non_linear_term, .x, covariates)) %>% return()

  }else if(model == "surv"){
    purrr::map(degrees, ~ write_single_formula.surv(non_linear_term, .x, covariates)) %>% return()
  }else{
    errorCondition("Please use 'surv' or 'polynomial' for model")
  }
}

create_degree_combinations_all = function(max_degree, ncomp, degree_sets = "matched", model, approach) {
  if(approach == "reduced"){
    ncomp_adjusted = ncomp - 1
  }else{
    ncomp_adjusted = ncomp
  }

  if (ncomp_adjusted > 1 & degree_sets == "independent") {
    deg_table = degree_combinations_independent(max_degree, model, ncomp_adjusted)
  } else{
    deg_table = map_dfc(1:ncomp_adjusted, ~degree_tib_all(.x, max_degree, model))
    }
  purrr::transpose(deg_table) %>% map(., unlist) %>% map(., unname) %>% return()
}

degree_tib_all = function(i, max_degree, model){
  if(model == "surv"){
    min_degree = 2
  }else{
    min_degree = 1
  }
  a = tibble(placeholder = min_degree:max_degree)
  colnames(a) <- paste0("comp", i, "_degree")
  return(a)
}

degree_combinations_independent =
  function(max_degree, model, ncomp) {
    table_intermediate = (degree_tib_all(1, max_degree, model))
    for (i in 2:ncomp) {
      table_intermediate %<>%
        reframe(.by = everything(),
                degree_tib_all(i, max_degree, model))
    }
    return(table_intermediate)
  }

add_single_degree_to_fold_likelihood = function(df, i, j, nfolds){
  tibble(!!paste0("degree_", i) := rep(j,nfolds))
}

add_all_degrees_to_fold_likelihood = function(df, degrees, nfolds){
  map2_dfc(1:length(degrees), degrees, ~add_single_degree_to_fold_likelihood(df, .x, .y, nfolds))
}

calculate_fold_likelihood_all = function(testing_set,
                                         trained_mu_model,
                                         trained_pi_model,
                                         approach = "full",
                                         fixed_side = NULL,
                                         extra_row = FALSE) {
  if (approach == "full") {
    calculate_fold_likelihood.full(testing_set, trained_mu_model, trained_pi_model) %>% return()
  } else if (approach == "reduced" & !is.null(fixed_side)) {
    calculate_fold_likelihood.reduced(testing_set,
                                      trained_mu_model,
                                      trained_pi_model,
                                      fixed_side,
                                      extra_row) %>% return()
  } else{
    errorCondition("If using reduced model, need to supply a value for fixed_side")
  }
}

calculate_fold_likelihood.full = function(testing_set, trained_mu_model, trained_pi_model){

  testing_set %>% add_obs_id() %>% reframe(.by = everything(),
                                           c = as.character(1:2)) %>%
    calculate_density_obs(., trained_mu_model) %>%
    pi_model_predictions(., trained_pi_model) %>%
    calculate_new_weights(.) %>%
    calculate_log_likelihood(.) %>% return()
}

calculate_fold_likelihood.reduced = function(testing_set, trained_mu_model, trained_pi_model, fixed_side, extra_row){
  testing_set %>% add_obs_id() %>% reframe(.by = everything(),
                                           c = as.character(1:2)) %>%
    calculate_density_obs_reduced(., trained_mu_model, fixed_side, extra_row) %>%
    pi_model_predictions(., trained_pi_model) %>%
    calculate_new_weights(.) %>%
    calculate_log_likelihood(.) %>% return()



}




