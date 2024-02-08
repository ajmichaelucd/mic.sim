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
#' @return
#' @export
#'
#' @examples
full_polynomial_cv = function(max_degree,
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
  deg_table = dplyr::cross_join(tibble(comp1_degree = 1:max_degree),
                                tibble(comp2_degree = 1:max_degree))
  transpose(deg_table) %>% map(., unlist) %>% map(., unname) %>%
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
  visible_data = visible_data %>% mutate(fold = sample(rep(1:10, each = (
    nrow(visible_data) / 10
  ) %>% ceiling), nrow(visible_data)))

  tibble(fold_likelihood = map_dbl(
    1:nfolds,
    ~ get_fold_likelihood(
      .x,
      visible_data,
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

  mu_formula = list(
    reformulate(
      termlabels = c(
        paste0("poly(", non_linear_term, ",", "degree = ", degrees[1], ")"),
        covariates
      ),
      response = "Surv(time = left_bound, time2 = right_bound, type = 'interval2')"
    ),
    reformulate(
      termlabels = c(
        paste0("poly(", non_linear_term, ",", "degree = ", degrees[2], ")"),
        covariates
      ),
      response = "Surv(time = left_bound, time2 = right_bound, type = 'interval2')"
    )
  )
  ##add check for if fold column exists
  training_set = visible_data %>% filter(fold != test)
  testing_set = visible_data %>% filter(fold == test)

  trained_model = EM_algorithm(
    training_set,
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




  testing_set %>% reframe(.by = everything(),
                          c = as.character(1:2)) %>%
    mutate(
      `E[Y|t,c]` = case_when(
        c == "1" ~ predict(trained_model$mu_model[[1]], newdata = .),
        c == "2" ~ predict(trained_model$mu_model[[2]], newdata = .),
        TRUE ~ NaN
      ),
      #predict(model, newdata = possible_data),
      `sd[Y|t,c]` = case_when(
        c == "1" ~ trained_model$mu_model[[1]]$scale,
        c == "2" ~ trained_model$mu_model[[2]]$scale,
        #1,
        TRUE ~ NaN
      ),
      #model$scale[c], #####QUESTION HERE????????????????????????????
      # `Var[Y|t,c]` = `sd[Y|t,c]`^2,

      `P(Y|t,c)` = case_when(
        left_bound == right_bound ~ dnorm(x = left_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`),
        left_bound <= `E[Y|t,c]` ~ pnorm(right_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`) -
          pnorm(left_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`),
        TRUE ~ pnorm(
          left_bound,
          mean = `E[Y|t,c]`,
          sd =  `sd[Y|t,c]`,
          lower.tail = FALSE
        ) -
          pnorm(
            right_bound,
            mean = `E[Y|t,c]`,
            sd =  `sd[Y|t,c]`,
            lower.tail = FALSE
          )
      ),
      `P(C=c|t)` = case_when(
        c == "2" ~ predict(
          trained_model$pi_model,
          newdata = tibble(t = t),
          type = "response"
        ),
        c == "1" ~ 1 - predict(
          trained_model$pi_model,
          newdata = tibble(t = t),
          type = "response"
        )
      ),
      `P(c,y|t)` = `P(C=c|t)` * `P(Y|t,c)`
    ) %>%
    mutate(.by = obs_id,
           `P(Y=y|t)` = sum(`P(c,y|t)`)) %>%
    mutate(`P(C=c|y,t)` = `P(c,y|t)` / `P(Y=y|t)`) %>%
    calculate_log_likelihood(.)
}
