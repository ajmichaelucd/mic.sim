#' Title
#'
#' @param max_degree
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
#' @export
#'
#' @examples
fit_polynomial_EM = function(max_degree,
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
                             sd_initial = 0.2){


  cv_results = full_polynomial_cv(max_degree = 8, visible_data = visible_data %>% mutate(obs_id = row_number()), nfolds = 10, verbose = 2) %>%
    summarize(.by = c(degree_1, degree_2), log_likelihood = sum(fold_likelihood)) %>%
    arrange(desc(log_likelihood))

  degree_1 = cv_results %>% head(1) %>% pull(degree_1)
  degree_2 =  cv_results %>% head(1) %>% pull(degree_2)

  mu_formula = list(
    reformulate(
      termlabels = c(
        paste0("poly(", non_linear_term, ",", "degree = ", degree_1, ")", covariates),
        covariates
      ),
      response = "Surv(time = left_bound, time2 = right_bound, type = 'interval2')"
    ),
    reformulate(
      termlabels = c(
        paste0("poly(", non_linear_term, ",", "degree = ", degree_2, ")", covariates),
        covariates
      ),
      response = "Surv(time = left_bound, time2 = right_bound, type = 'interval2')"
    )
  )

  output = EM_algorithm(
    visible_data,
    model = "polynomial",
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
