#' Title
#'
#' @param max_degree
#' @param visible_data
#' @param nfolds
#' @param mu_formula
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
#' @importFrom purrr transpose
#'
#' @return
#' @export
#'
#' @examples
full_polynomial_cv = function(max_degree,
                              degree_sets,
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

  create_degree_combinations(max_degree, ncomp, degree_sets) %>%
    map(
      .,
      ~ single_cv(
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

single_cv = function(degrees,
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
    ~ get_fold_likelihood(
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

get_fold_likelihood = function(i,
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

  mu_formula = write_all_polynomial_formulas(non_linear_term, degrees, covariates)

  ##add check for if fold column exists
  training_set = visible_data %>% filter(fold != test)
  testing_set = visible_data %>% filter(fold == test)

  trained_model = EM_algorithm(
    visible_data = training_set,
    model = "polynomial",
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


create_degree_combinations = function(max_degree, ncomp, degree_sets = "matched") {
  if (ncomp == 2 & degree_sets == "independent") {
    deg_table = dplyr::cross_join(degree_tib(1, max_degree),
                                  degree_tib(2, max_degree))
  } else if (ncomp == 2 & degree_sets == "matched") {
    deg_table = tibble(degree_tib(1, max_degree),
                       degree_tib(2, max_degree))
  } else{
    deg_table = tibble(degree_tib(1, max_degree))
  }
  purrr::transpose(deg_table) %>% map(., unlist) %>% map(., unname) %>% return()
}



write_single_polynomial_formula = function(non_linear_term, degrees, covariates){
  reformulate(
    termlabels = c(
      paste0("poly(", non_linear_term, ",", "degree = ", degrees, ")"),
      covariates
    ),
    response = "Surv(time = left_bound, time2 = right_bound, type = 'interval2')"
  )
}

write_all_polynomial_formulas = function(non_linear_term, degrees, covariates){
  purrr::map(degrees, ~ write_single_polynomial_formula(non_linear_term, .x, covariates))
}

degree_tib = function(i, max_degree){
  a = tibble(placeholder = 1:max_degree)
  colnames(a) <- paste0("comp", i, "_degree")
  return(a)
}

assign_folds = function(visible_data, nfolds){
  visible_data = visible_data %>% ungroup() %>% mutate(fold =
                                           sample(
                                             rep(1:nfolds, each = (
                                                nrow(visible_data) / nfolds
                                                  %>% ceiling)), nrow(visible_data)))
}

calculate_fold_likelihood = function(testing_set, trained_mu_model, trained_pi_model){
  testing_set %>% reframe(.by = everything(),
                          c = as.character(1:2)) %>%
    calculate_density_obs(., trained_mu_model) %>%
    pi_model_predictions(., trained_pi_model) %>%
    calculate_new_weights(.) %>%
    calculate_log_likelihood(.)
}
