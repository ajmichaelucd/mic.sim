# mu_model = function(possible_data, pred_comp){
#   possible_data %>% filter(c == pred_comp & `P(C=c|y,t)` > 0) %>%
#     mutate(`P(C=c|y,t)` =  paste0(`P(C=c|y,t)`) %>% as.numeric) %>%
#     survival::survreg(Surv(time = left_bound,
#                            time2 = right_bound,
#                            type = "interval2") ~ poly(t, degree = 4),
#                       weights = `P(C=c|y,t)`,
#                       data = .,
#                       dist = "gaussian") %>% return()
# }

visible_data
degree = 4

#nrow(visible_data) %% 10
#nrow(visible_data) %/% 10
#
#function(visible_data, n_fold)
#  visible_data = visible_data %>% mutate(fold = NaN)
#  remaining = tibble(ids = 1:nrow(visible_data))
#
#for(i in 1:n_fold){
#n_each = ifelse(i <=  nrow(visible_data) %% n_fold, (nrow(visible_data) %/% n_fold) + 1, nrow(visible_data) %/% n_fold)
#fold_i = sample(remaining$ids, n_each, replace = FALSE)
#visible_data = visible_data %>% mutate(.by = obs_id, fold = case_when(
#    obs_id %in% fold_i ~ i,
#    TRUE ~ fold
#  )
#)
#remaining = remaining %>% filter(!(ids %in% fold_i))
#
#}

#fold = sample(remaining$ids, )

#loop over degrees
#degree = 4


max_degree = 8


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


cv_results = full_polynomial_cv(max_degree = 8, visible_data = example_data %>% mutate(obs_id = row_number()), nfolds = 10, verbose = 2) %>%
  summarize(.by = c(degree_1, degree_2), log_likelihood = sum(fold_likelihood)) %>%
  arrange(desc(log_likelihood))

degree_1 = cv_results %>% head(1) %>% pull(degree_1)
degree_2 =  cv_results %>% head(1) %>% pull(degree_2)

mu_formula = list(
  reformulate(
    termlabels = c(
      paste0("poly(", non_linear_term, ",", "degree = ", degree_1, ")", mu_covariates),
      covariates
    ),
    response = "Surv(time = left_bound, time2 = right_bound, type = 'interval2')"
  ),
  reformulate(
    termlabels = c(
      paste0("poly(", non_linear_term, ",", "degree = ", degree_2, ")", mu_covariates),
      covariates
    ),
    response = "Surv(time = left_bound, time2 = right_bound, type = 'interval2')"
  )
)

output = EM_algorithm(
  example_data,
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

m = fit_polynomial_EM(max_degree, visible_data, nfolds)

full_polynomial_cv = function(max_degree, visible_data, nfolds){
map(1:max_degree, ~single_cv(degree = .x, visible_data = visible_data, nfolds = nfolds)) %>% data.table::rbindlist() %>% tibble %>% return()
}

single_cv = function(degree, visible_data, nfolds){
visible_data = visible_data %>% mutate(fold = sample(rep(1:10, each = (nrow(visible_data)/10) %>% ceiling), nrow(visible_data)))

tibble(fold_likelihood = map_dbl(1:nfolds, ~ get_fold_likelihood(.x, visible_data))) %>%  mutate(fold = row_number(),
                                                                            degree = degree) %>% return()
}

get_fold_likelihood = function(i, visible_data){
test = i
training_set = visible_data %>% filter(fold != test)
testing_set = visible_data %>% filter(fold == test)

trained_model = EM_algorithm(
    training_set,
    model = "surv", #"mgcv", "polynomial"
    mu_formula = Surv(time = left_bound, time2 = right_bound, type = "interval2") ~
      poly(t, degree = 4),
    #mu_formula = yi ~ s(t),
    pi_formula = c == "2" ~ s(t), #or: c == "2" ~ lo(t)
    max_it = 3000,
    ncomp = 2,
    tol_ll = 1e-6,
    browse_at_end = FALSE,
    browse_each_step = FALSE,
    plot_visuals = FALSE,
    prior_step_plot = FALSE,
    pause_on_likelihood_drop = FALSE,
    pi_link = "logit",
    verbose = 3,
    model_coefficient_tolerance = 0.00001,
    maxiter_survreg = 30,
    initial_weighting = 8,
    sd_initial = 0.2,
    stop_on_likelihood_drop = FALSE,
    n_models = 100,
    seed = NULL,
    randomize = "all"
)


testing_set %>% reframe(.by = everything(),
                       c = as.character(1:2)) %>%
  mutate(
    `E[Y|t,c]` = case_when(c == "1" ~ predict(trained_model$mu_model[[1]], newdata = .),
                           c == "2" ~ predict(trained_model$mu_model[[2]], newdata = .),
                           TRUE ~ NaN),
    #predict(model, newdata = possible_data),
    `sd[Y|t,c]` = case_when(c == "1" ~ trained_model$mu_model[[1]]$scale,
                            c == "2" ~ trained_model$mu_model[[2]]$scale, #1,
                            TRUE ~ NaN),
    #model$scale[c], #####QUESTION HERE????????????????????????????
    # `Var[Y|t,c]` = `sd[Y|t,c]`^2,

    `P(Y|t,c)` = case_when(
      left_bound == right_bound ~ dnorm(x = left_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`),
      left_bound <= `E[Y|t,c]` ~ pnorm(right_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`) -
        pnorm(left_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`),
      TRUE ~ pnorm(left_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`, lower.tail = FALSE) -
        pnorm(right_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`, lower.tail = FALSE)
    ),
    `P(C=c|t)` = case_when(
      c == "2" ~ predict(trained_model$pi_model, newdata = tibble(t = t), type = "response"),
      c == "1" ~ 1 - predict(trained_model$pi_model, newdata = tibble(t = t), type = "response")
    ),
    `P(c,y|t)` = `P(C=c|t)` * `P(Y|t,c)`
  ) %>%
  mutate(.by = obs_id,
         `P(Y=y|t)` = sum(`P(c,y|t)`)) %>%
  mutate(
    `P(C=c|y,t)` = `P(c,y|t)` / `P(Y=y|t)`) %>%
  calculate_log_likelihood(.)
}



optimal_degree = results %>% head(1) %>% pull(degree)


