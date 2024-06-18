output = EM_algorithm_reduced(
  fixed_side = "LC",
  extra_row = FALSE,
  visible_data,
  model = "surv", #"mgcv", "polynomial"
  mu_formula = Surv(time = left_bound,
                    time2 = right_bound,
                    type = "interval2") ~ pspline(t, df = 5),
  #mu_formula = yi ~ s(t),
  pi_formula = c == "2" ~ s(t), #or: c == "2" ~ lo(t)
  max_it = 3000,
  ncomp = 2,
  tol_ll = 1e-6,
  browse_at_end = FALSE,
  browse_each_step = FALSE,
  plot_visuals = FALSE,
  prior_step_plot = FALSE,
  pause_on_likelihood_drop = TRUE,
  pi_link = "logit",
  verbose = 3,
  model_coefficient_tolerance = 0.00001,
  maxiter_survreg = 30,
  initial_weighting = 2,
  sd_initial = 0.2,
  stop_on_likelihood_drop = FALSE,
  non_linear_term = "t",
  covariates = NULL
)

output


EM_algorithm_reduced = function(
    #add:
  fixed_side = "RC",
  extra_row = FALSE,



    visible_data,
    model = "surv", #"mgcv", "polynomial"
    mu_formula = Surv(time = left_bound,
                      time2 = right_bound,
                      type = "interval2") ~ pspline(t, df = 0, caic = TRUE),
    #mu_formula = yi ~ s(t),
    pi_formula = c == "2" ~ s(t), #or: c == "2" ~ lo(t)
    max_it = 3000,
    ncomp = 2,
    tol_ll = 1e-6,
    browse_at_end = FALSE,
    browse_each_step = FALSE,
    plot_visuals = FALSE,
    prior_step_plot = FALSE,
    pause_on_likelihood_drop = TRUE,
    pi_link = "logit",
    verbose = 3,
    model_coefficient_tolerance = 0.00001,
    maxiter_survreg = 30,
    initial_weighting = 1,
    sd_initial = 0.2,
    stop_on_likelihood_drop = FALSE,
    #cut n_models = 100,
    #cut seed = NULL,
    # cut randomize = "all",
    non_linear_term = "t",
    covariates = NULL
){
  #add attribute model to visible data

  visible_data = modify_visible_data(visible_data, model)

  visible_data = set_scale_log(visible_data, scale)


  converge = NA_character_

  if(ncomp == 1){
      errorCondition("The reduced model is appropriate for more than 1 component")
    }else{

    #first E step-----
    possible_data = first_E_step_reduced(initial_weighting, visible_data, plot_visuals, sd_initial, ncomp, non_linear_term, covariates, pi_formula, max_it,tol_ll, pi_link, model_coefficient_tolerance)

    #wrapper function
    #plot_initial_weighting_regression(possible_data = possible_data)
    #browser?
    likelihood_documentation = set_up_likelihood_matrix(max_it)
    possible_data = modify_bounds(possible_data)



    for(i in 1:max_it){
      message_iteration_count(i, verbose)
      #first M step--------
      #MLE of all parameters

      if(i != 1){
        prior_iteration = save_previous_iteration(mu_models_new, pi_model_new, log_likelihood_new, possible_data)
      }

    mu_models_new = fit_all_mu_models_v2(possible_data = possible_data, ncomp = ncomp, mu_formula = mu_formula, fixed_side = fixed_side, maxiter_survreg = maxiter_survreg)
      #mu_models_new = fit_all_mu_models(possible_data, ncomp, mu_formula, maxiter_survreg)
      #likelihood_documentation = M_step_likelihood_matrix_updates(likelihood_documentation, i, mu_models_new, ncomp, maxiter_survreg) ###use dimnames in matrix
      if(check_mu_models_convergence(mu_models_new, ncomp - 1)){
        converge = "NO"
        break #if wrapping into an M-step function, add list to list of outputs, then check between steps
      }

      pi_model_new = fit_mgcv_pi_model(pi_formula = pi_formula, pi_link = pi_link, possible_data = possible_data)

      if(i > 1){
        model_coefficient_checks_results = model_coefficient_checks(mu_models_new, pi_model_new, prior_iteration$mu_models_old, prior_iteration$pi_model_old, model_coefficient_tolerance, ncomp-1)
      }




      #Next E step-------------

      # likelihood_documentation[i, "m_step_check_new"] <- m_step_check_maximizing(possible_data, mu_models_new, pi_model_new) ###use dimnames in matrix
      # if(i > 1){
      #   likelihood_documentation[i, "m_step_check_old"] <- m_step_check_maximizing(possible_data, prior_iteration$mu_models_old, prior_iteration$pi_model_old) ###use dimnames in matrix
      # }else{
      #   likelihood_documentation[i, "m_step_check_old"] <- NaN
      # }

      possible_data = E_step_reduced(possible_data, mu_models_new, pi_model_new, fixed_side = fixed_side, extra_row = extra_row)

      if(verbose > 2){
        print(pi_model_new)
        print(mu_models_new)
      }



      log_likelihood_new = calculate_log_likelihood(possible_data, verbose)
      likelihood_documentation[i, "loglikelihood"] <- log_likelihood_new ###use dimnames

      if(i > 1 && likelihood_documentation[i, "loglikelihood"] < likelihood_documentation[i - 1, "loglikelihood"] & stop_on_likelihood_drop){
        converge = "likelihood decreased"
        break
      }

      if(i > 1 & pause_on_likelihood_drop && likelihood_documentation[i, "loglikelihood"] < likelihood_documentation[i - 1, "loglikelihood"]){
        browser("likelihood decreased")
      }

      if(browse_each_step & plot_visuals){

        browser(message("End of step ", i))
        plot_fm_step(pi_model_new, mu_models_new, ncomp, possible_data, prior_step_plot, i)

        if(i > 1){
          plot_likelihood(likelihood_documentation, format = "matrix")
        }
      }

      if(browse_each_step & !plot_visuals){browser(message("End of step ", i))}

      if(i != 1){
        ##too much overlap with 103
        check_ll = (log_likelihood_new - prior_iteration$log_likelihood_old)

        if(check_ll < 0){
          warning("Log Likelihood decreased")  ###  HAS BEEN GETTING USED A LOT, WHY IS THE LOG LIKELIHOOD GOING DOWN????
        }



        if(check_ll < tol_ll & model_coefficient_checks_results)
        {
          if(verbose > 0){
            message("Stopped on combined LL and parameters")}
          converge = "YES"
          break
        }

      }



    }


    ####group 149 to 173
    if(i > 1){
      if(i == max_it & !(check_ll < tol_ll & model_coefficient_checks_results)){
        converge = "iterations"
      }
    }

    if(i == 1 & i == max_it & is.na(converge)){
      converge = "iterations"
    }


    if(i == 1){
      prior_iteration = list(
        mu_models_old = NA,
        pi_model_old = NA,
        log_likelihood_old = NA,
        possible_data_old = NA
      )
    }

    if(initial_weighting == 7){
      random_start_set = randomize
    }else{
      random_start_set = NA_character_
    }

    return(
      list(
        likelihood = tibble_like(likelihood_documentation),
        model = model,
        possible_data = possible_data,
        pi_model = pi_model_new,
        mu_model = mu_models_new,
        steps = i,
        converge = converge,
        ncomp = ncomp,
        prior_step_models = prior_iteration,
        #seed = seed,
        random_start_set = random_start_set,
        sd_initial = sd_initial,  # ifelse(initial_weighting >= 7, sd_initial, NaN),
        mu_formula = mu_formula,
        fixed_side = fixed_side,
        extra_row = extra_row
      )
    )
  }
}
















calculate_density_obs_reduced = function(possible_data, mu_models, fixed_side, extra_row){
  if(fixed_side == "RC" & !extra_row){
    possible_data %>%
      mutate(
        `E[Y|t,c]` = if_else(c == 1, predict(mu_models[[1]], newdata = possible_data), NA_real_),
        `sd[Y|t,c]` = if_else(c == 1, mu_models[[1]]$scale, NA_real_),
        `P(Y|t,c)` = case_when(
          c == 1 & left_bound == right_bound ~ dnorm(x = left_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`),
          c == 1 & left_bound <= `E[Y|t,c]` ~ pnorm(right_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`) -
            pnorm(left_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`),
          c == 1 ~ pnorm(left_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`, lower.tail = FALSE) -
            pnorm(right_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`, lower.tail = FALSE),
          TRUE ~ (right_bound == Inf) %>% as.numeric()
        )
      )
  }else if(fixed_side == "LC" & !extra_row){
    possible_data %>%
      mutate(
        `E[Y|t,c]` = if_else(c == 2, predict(mu_models[[1]], newdata = possible_data), NA_real_),
        `sd[Y|t,c]` = if_else(c == 2, mu_models[[1]]$scale, NA_real_),
        `P(Y|t,c)` = case_when(
          c == 2 & left_bound == right_bound ~ dnorm(x = left_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`),
          c == 2 & left_bound <= `E[Y|t,c]` ~ pnorm(right_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`) -
            pnorm(left_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`),
          c == 2 ~ pnorm(left_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`, lower.tail = FALSE) -
            pnorm(right_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`, lower.tail = FALSE),
          TRUE ~ (left_bound == -Inf ) %>% as.numeric()
        )
      )
  }else if(fixed_side == "RC" & extra_row){
    possible_data %>%
      mutate(
        `E[Y|t,c]` = if_else(c == 1, predict(mu_models[[1]], newdata = possible_data), NA_real_),
        `sd[Y|t,c]` = if_else(c == 1, mu_models[[1]]$scale, NA_real_),
        `P(Y|t,c)` = case_when(
          c == 1 & left_bound == right_bound ~ dnorm(x = left_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`),
          c == 1 & left_bound <= `E[Y|t,c]` ~ pnorm(right_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`) -
            pnorm(left_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`),
          c == 1 ~ pnorm(left_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`, lower.tail = FALSE) -
            pnorm(right_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`, lower.tail = FALSE),
          TRUE ~ (right_bound == Inf | right_bound == high_con) %>% as.numeric()
        )
      )
  }else if(fixed_side == "LC" & extra_row){
    possible_data %>%
      mutate(
        `E[Y|t,c]` = if_else(c == 2, predict(mu_models[[1]], newdata = possible_data), NA_real_),
        `sd[Y|t,c]` = if_else(c == 2, mu_models[[1]]$scale, NA_real_),
        `P(Y|t,c)` = case_when(
          c == 2 & left_bound == right_bound ~ dnorm(x = left_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`),
          c == 2 & left_bound <= `E[Y|t,c]` ~ pnorm(right_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`) -
            pnorm(left_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`),
          c == 2 ~ pnorm(left_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`, lower.tail = FALSE) -
            pnorm(right_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`, lower.tail = FALSE),
          TRUE ~ (left_bound == -Inf | left_bound == low_con) %>% as.numeric()
        )
      )
  }else{
    errorCondition("Invalid value for either fixed_side or extra_row")
  }

}

E_step_reduced = function(possible_data, mu_models, pi_model, fixed_side, extra_row){
  possible_data %<>% calculate_density_obs_reduced(., mu_models, fixed_side, extra_row) %>%
    pi_model_predictions(., pi_model) %>%
    calculate_new_weights() %>% return()
}


first_E_step_reduced = function(initial_weighting, visible_data, plot_visuals, sd_initial = 0.2, ncomp = 2, non_linear_term, covariates, pi_formula, max_it, tol_ll, pi_link, model_coefficient_tolerance){
  if(initial_weighting == 1){
    possible_data = initial_weighting_reduced(visible_data = visible_data, fixed_side = fixed_side, extra_row = extra_row )
  }else{
    linear_e_step_output = initial_weighting_fit_linear_model_reduced(visible_data, fixed_side, extra_row = extra_row, non_linear_term, covariates, pi_formula, max_it, ncomp, tol_ll, pi_link, verbose, model_coefficient_tolerance, sd_initial)
    possible_data = linear_e_step_output$possible_data
  }


  attr(possible_data, "plot_initial") <- (plot_visuals & initial_weighting == 8)
  possible_data = add_attribute_data(possible_data, model)
  possible_data = modify_bounds(possible_data)
  return(possible_data)
}

initial_weighting_reduced = function(visible_data, fixed_side, extra_row){
  if(fixed_side == "RC" & !extra_row){
    possible_data =
      visible_data %>% #visible data with c for component
      reframe(.by = everything(),    #implement for other intial weighting options too ##########
              c = as.character(1:2) #fir a logistic regression on c earlier #########
              # `P(C=c|y,t)` = LearnBayes::rdirichlet(1, rep(.1, ncomp)) %>% as.vector(),
              #       .groups = "drop"
      ) %>%
      mutate(
        `P(C=c|y,t)` = case_when(right_bound == Inf & c == "1"~ 0.01 ,
                                 right_bound == Inf & c == "2"~ 0.99 ,
                                 right_bound != Inf & c == "1"~ 1 ,
                                 right_bound != Inf & c == "2"~ 0) #could mess with the cutoff to redefine which observations go in the abnormal group, e.g. new cutoff instead of right_bound == Inf could be left_bound  == ?
        ###Also only works with scale == "log"
      )} else if(fixed_side == "LC" & !extra_row){ #%>%
        possible_data =
          visible_data %>% #visible data with c for component
          reframe(.by = everything(),    #implement for other intial weighting options too ##########
                  c = as.character(1:2) #fir a logistic regression on c earlier #########
                  # `P(C=c|y,t)` = LearnBayes::rdirichlet(1, rep(.1, ncomp)) %>% as.vector(),
                  #       .groups = "drop"
          ) %>%
          mutate(
            `P(C=c|y,t)` = case_when(right_bound == low_con & c == "1"~ 0.99 ,
                                     right_bound == low_con & c == "2"~ 0.01 ,
                                     right_bound != low_con & c == "1"~ 0 ,
                                     right_bound != low_con & c == "2"~ 1) #could mess with the cutoff to redefine which observations go in the abnormal group, e.g. new cutoff instead of right_bound == Inf could be left_bound  == ?
            ###Also only works with scale == "log"
          )}else if(fixed_side == "LC" & extra_row){ #%>%
            possible_data =
              visible_data %>% #visible data with c for component
              reframe(.by = everything(),    #implement for other intial weighting options too ##########
                      c = as.character(1:2) #fir a logistic regression on c earlier #########
                      # `P(C=c|y,t)` = LearnBayes::rdirichlet(1, rep(.1, ncomp)) %>% as.vector(),
                      #       .groups = "drop"
              ) %>%
              mutate(
                `P(C=c|y,t)` = case_when((right_bound == low_con | left_bound == low_con) & c == "1"~ 0.99 ,
                                         (right_bound == low_con | left_bound == low_con) & c == "2"~ 0.01 ,
                                         right_bound != low_con & left_bound != low_con & c == "1"~ 0 ,
                                         right_bound != low_con & left_bound != low_con & c == "2"~ 1) #could mess with the cutoff to redefine which observations go in the abnormal group, e.g. new cutoff instead of right_bound == Inf could be left_bound  == ?
                ###Also only works with scale == "log"
              )}else if(fixed_side == "RC" & extra_row){
                possible_data =
                  visible_data %>% #visible data with c for component
                  reframe(.by = everything(),    #implement for other intial weighting options too ##########
                          c = as.character(1:2) #fir a logistic regression on c earlier #########
                          # `P(C=c|y,t)` = LearnBayes::rdirichlet(1, rep(.1, ncomp)) %>% as.vector(),
                          #       .groups = "drop"
                  ) %>%
                  mutate(
                    `P(C=c|y,t)` = case_when((right_bound == Inf | right_bound == high_con) & c == "1"~ 0.01 ,
                                             (right_bound == Inf | right_bound == high_con) & c == "2"~ 0.99 ,
                                             right_bound != Inf & right_bound != high_con & c == "1"~ 1 ,
                                             right_bound != Inf & right_bound != high_con & c == "2"~ 0) #could mess with the cutoff to redefine which observations go in the abnormal group, e.g. new cutoff instead of right_bound == Inf could be left_bound  == ?
                    ###Also only works with scale == "log"
                  )}else{
                    TRUE ~ errorCondition("fixed_side must be 'RC' or 'LC' and extra_row must be logical")
                  }
  possible_data %>% return()
}

fit_all_mu_models_v2 = function(possible_data, ncomp, mu_formula, fixed_side = NULL, maxiter_survreg = 30){
  fitted_comp = case_when(
    is.null(fixed_side) ~ c(1:ncomp),
    !is.null(fixed_side) && fixed_side == "LC" ~ c(2),
    !is.null(fixed_side) && fixed_side == "RC" ~ c(1),
    TRUE ~ -1
  ) %>% unique()
  if(length(fitted_comp) == 1 && fitted_comp < 0){
    errorCondition("Invalid value of fixed side, use 'LC', 'RC', or 'NULL'")
  }
  if(!is.list(mu_formula)){
    mu_formula = list(mu_formula)
  }
  if(length(mu_formula) < length(fitted_comp) && ((length(fitted_comp) %% length(mu_formula)) %% length(mu_formula)) == 0){
    message(length)
    rep(mu_formula, (length(fitted_comp) / length(mu_formula)) )
  }

  if(is.list(mu_formula) && length(mu_formula) == length(fitted_comp)){

    if(attr(possible_data, "model") %in% c("surv", "polynomial")){
      mu_models_new = purrr::map2(fitted_comp, mu_formula, ~fit_mu_model(possible_data = possible_data, pred_comp = .x, mu_formula = .y, maxiter_survreg = maxiter_survreg))
      mu_models_new = purrr::map(mu_models_new, ~set_model_attr(.x, possible_data))
      attr(mu_models_new, "model") <- attr(possible_data, "model")
    }else if(attr(possible_data, "model") == "mgcv"){
      mu_models_new = purrr::map2(fitted_comp, mu_formula, ~fit_mu_model.mgcv(possible_data = possible_data, pred_comp = .x, mu_formula = .y))
      mu_models_new = purrr::map(mu_models_new, ~set_model_attr(.x, possible_data))
      attr(mu_models_new, "model") <- attr(possible_data, "model")
    }else{
      errorCondition("model must be 'surv', 'polynomial', or 'mgcv'")
    }

  }else{
    errorCondition("mu_formula should be a list of formulas equal to the number of component means being estimated or a single formula to be repeated for all the components")
  }

  attr(mu_models_new, "fixed_side") <- fixed_side
  return(mu_models_new)
}

initial_weighting_fit_linear_model_reduced = function(visible_data, fixed_side, extra_row = extra_row, non_linear_term, covariates, pi_formula, max_it, ncomp, tol_ll, pi_link, verbose, model_coefficient_tolerance, sd_initial){
  mu_formula = write_all_polynomial_formulas(non_linear_term = non_linear_term, degrees = rep(1, ncomp - 1), covariates = covariates)

  visible_data %>%
    EM_algorithm_reduced(
      fixed_side = fixed_side,
      extra_row = extra_row,
      visible_data = .,
      mu_formula = mu_formula,
      pi_formula = pi_formula,
      max_it = max_it,
      ncomp = ncomp,
      tol_ll = tol_ll,
      pi_link = pi_link,
      verbose = verbose,
      model_coefficient_tolerance = model_coefficient_tolerance,
      initial_weighting = 1,
      sd_initial = sd_initial
    ) %>% return()
}

