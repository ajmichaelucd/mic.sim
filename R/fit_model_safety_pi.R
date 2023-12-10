#' fit_modek_safety_pi
#'
#' @param visible_data
#' @param formula
#' @param formula2
#' @param censored_side
#' @param max_it
#' @param ncomp
#' @param tol_ll
#' @param browse_at_end
#' @param browse_each_step
#' @param plot_visuals
#' @param pi_link
#' @param verbose
#' @param maxiter_survreg
#'
#' @importFrom survival pspline survreg Surv coxph.wtest
#' @importFrom splines ns
#' @importFrom ggplot2 geom_function aes ggplot
#' @importFrom tidyr pivot_wider
#'
#' @return
#' @export
#'
#' @examples
fit_model_safety_pi = function(
    visible_data = prep_sim_data_for_em(),
    mu_formula = Surv(time = left_bound,
                   time2 = right_bound,
                   type = "interval2") ~ pspline(t, df = 0, calc = TRUE),
    pi_formula2 = c == "2" ~ s(t),
    censored_side = "RC",
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
    stop_on_likelihood_drop = TRUE,
    extra_row = FALSE){
  #verbose = 0: print nothing
  #verbose = 1: print run number (controlled outside in the purrr::map of this) --done
  #verbose = 2: print run number and iteration number --done
  #verbose = 3: print run number, iteration number, and iteration results --done
  #verbose = 4: print run number, iteration number, iteration results, and run aft as verbose
  #verbose = 0:
  if(ncol(visible_data %>% select(matches("obs_id"))) == 0){
    visible_data = visible_data %>% mutate(obs_id = row_number()) %>% select(obs_id, everything())
  }


  median_y = median(visible_data$left_bound)
  #first E step-----
  #possible_data = case_when(

possible_data = initial_weighting_safety(visible_data, censored_side, extra_row)


  likelihood_documentation <- matrix(data = NA, nrow = max_it, ncol = 3)
  likelihood_documentation [,1] <- 1:max_it



  for(i in 1:max_it){
    if(verbose > 1){
      message("starting iteration number ", i)}
    if(verbose > 3){
      message("mem used = ")
      print(pryr::mem_used())
    }
    #first M step--------
    #MLE of all parameters
    if(i != 1){
      mu_model_old = mu_model_new
      pi_model_old = pi_model_new
      log_likelihood_old = log_likelihood_new
      possible_data_old = possible_data
    }

    if(censored_side == "RC") {mu_model_new = list(fit_mu_model(possible_data = possible_data, pred_comp = 1, mu_formula = mu_formula, maxiter_survreg = maxiter_survreg))
    }else if(censored_side == "LC") {mu_model_new = list(fit_mu_model(possible_data = possible_data, pred_comp = 2, mu_formula = mu_formula, maxiter_survreg = maxiter_survreg))
    }else{errorCondition("Invalid censored_side value")}


    if (mu_model_new[[1]]$iter[1] == maxiter_survreg){
      likelihood_documentation[i,3] <- TRUE
    } else{
      likelihood_documentation[i,3] <- FALSE
    }

    pi_model_new = fit_mgcv_pi_model(pi_formula = pi_formula, pi_link = pi_link, possible_data = possible_data)

    if(check_mu_models_convergence_surv(mu_model_new, ncomp - 1) %>% unlist %>% any){
      converge = "NO"
      break
    }

    if(i != 1){

      model_coefficient_checks_results = model_coefficient_checks_surv(mu_model_new, pi_model_new, mu_model_old, pi_model_old, model_coefficient_tolerance, ncomp - 1)

    }


    #Next E step-------------
    if(censored_side == "RC" & !extra_row){
    possible_data %<>%
      mutate(
        `E[Y|t,c]` = if_else(c == 1, predict(mu_model_new[[1]], newdata = possible_data), NA_real_),
        `sd[Y|t,c]` = if_else(c == 1, mu_model_new[[1]]$scale, NA_real_),
        `P(Y|t,c)` = case_when(
          c == 1 & left_bound == right_bound ~ dnorm(x = left_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`),
          c == 1 & left_bound <= `E[Y|t,c]` ~ pnorm(right_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`) -
            pnorm(left_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`),
          c == 1 ~ pnorm(left_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`, lower.tail = FALSE) -
            pnorm(right_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`, lower.tail = FALSE),
          TRUE ~ (right_bound == Inf) %>% as.numeric()
        ),
        `P(C=c|t)` = case_when(
          c == "2" ~ predict(pi_model_new, newdata = tibble(t = t), type = "response"),
          c == "1" ~ 1 - predict(pi_model_new, newdata = tibble(t = t), type = "response")
        ),
        `P(c,y|t)` = `P(C=c|t)` * `P(Y|t,c)`
      ) %>%
        mutate(.by = obs_id,
               `P(Y=y|t)` = sum(`P(c,y|t)`)) %>%
        mutate(
          `P(C=c|y,t)` = `P(c,y|t)` / `P(Y=y|t)`)
    } else if(censored_side == "LC" & !extra_row){
      possible_data %<>%
        #select(-any_of("P(C = c)")) %>%
        #left_join(pi, by = "c") %>%
        mutate(
          `E[Y|t,c]` = if_else(c == 2, predict(mu_model_new[[1]], newdata = possible_data), NA_real_),
          `sd[Y|t,c]` = if_else(c == 2, mu_model_new[[1]]$scale, NA_real_),
          `P(Y|t,c)` = case_when(
            c == 2 & left_bound == right_bound ~ dnorm(x = left_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`),
            c == 2 & left_bound <= `E[Y|t,c]` ~ pnorm(right_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`) -
              pnorm(left_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`),
            c == 2 ~ pnorm(left_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`, lower.tail = FALSE) -
              pnorm(right_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`, lower.tail = FALSE),
            TRUE ~ (left_bound == -Inf ) %>% as.numeric()
            ),
          `P(C=c|t)` = case_when(
            c == "2" ~ predict(pi_model_new, newdata = tibble(t = t), type = "response"),
            c == "1" ~ 1 - predict(pi_model_new, newdata = tibble(t = t), type = "response")
          ),
          `P(c,y|t)` = `P(C=c|t)` * `P(Y|t,c)`
        ) %>%
        mutate(.by = obs_id,
               `P(Y=y|t)` = sum(`P(c,y|t)`)) %>%
        mutate(
          `P(C=c|y,t)` = `P(c,y|t)` / `P(Y=y|t)`)
    } else if(censored_side == "RC" & extra_row){
      possible_data %<>%
        mutate(
          `E[Y|t,c]` = if_else(c == 1, predict(mu_model_new[[1]], newdata = possible_data), NA_real_),
          `sd[Y|t,c]` = if_else(c == 1, mu_model_new[[1]]$scale, NA_real_),
          `P(Y|t,c)` = case_when(
            c == 1 & left_bound == right_bound ~ dnorm(x = left_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`),
            c == 1 & left_bound <= `E[Y|t,c]` ~ pnorm(right_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`) -
              pnorm(left_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`),
            c == 1 ~ pnorm(left_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`, lower.tail = FALSE) -
              pnorm(right_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`, lower.tail = FALSE),
            TRUE ~ (right_bound == Inf | right_bound == high_con) %>% as.numeric()
          ),
          `P(C=c|t)` = case_when(
            c == "2" ~ predict(pi_model_new, newdata = tibble(t = t), type = "response"),
            c == "1" ~ 1 - predict(pi_model_new, newdata = tibble(t = t), type = "response")
          ),
          `P(c,y|t)` = `P(C=c|t)` * `P(Y|t,c)`
        ) %>%
        mutate(.by = obs_id,
               `P(Y=y|t)` = sum(`P(c,y|t)`)) %>%
        mutate(
          `P(C=c|y,t)` = `P(c,y|t)` / `P(Y=y|t)`)
    } else{
      possible_data %<>%
        #select(-any_of("P(C = c)")) %>%
        #left_join(pi, by = "c") %>%
        mutate(
          `E[Y|t,c]` = if_else(c == 2, predict(mu_model_new[[1]], newdata = possible_data), NA_real_),
          `sd[Y|t,c]` = if_else(c == 2, mu_model_new[[1]]$scale, NA_real_),
          `P(Y|t,c)` = case_when(
            c == 2 & left_bound == right_bound ~ dnorm(x = left_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`),
            c == 2 & left_bound <= `E[Y|t,c]` ~ pnorm(right_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`) -
              pnorm(left_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`),
            c == 2 ~ pnorm(left_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`, lower.tail = FALSE) -
              pnorm(right_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`, lower.tail = FALSE),
            TRUE ~ (left_bound == -Inf | left_bound == low_con) %>% as.numeric()
          ),
          `P(C=c|t)` = case_when(
            c == "2" ~ predict(pi_model_new, newdata = tibble(t = t), type = "response"),
            c == "1" ~ 1 - predict(pi_model_new, newdata = tibble(t = t), type = "response")
          ),
          `P(c,y|t)` = `P(C=c|t)` * `P(Y|t,c)`
        ) %>%
        mutate(.by = obs_id,
               `P(Y=y|t)` = sum(`P(c,y|t)`)) %>%
        mutate(
          `P(C=c|y,t)` = `P(c,y|t)` / `P(Y=y|t)`)
    }


    if(verbose > 2){
      print(pi_model_new)
      print(mu_model_new)
    }

    log_likelihood_new = calculate_log_likelihood(possible_data)
    likelihood_documentation[i, 2] <- log_likelihood_new

    if(verbose > 2){
      message(log_likelihood_new)
    }

    if(i > 1 && likelihood_documentation[i, 2] < likelihood_documentation[i - 1, 2] & stop_on_likelihood_drop){
      converge = "likelihood decreased"
      break
    }

    if(i > 1 && likelihood_documentation[i, 2] < likelihood_documentation[i - 1, 2]){
      browser("likelihood decreased")
    }



    if(browse_each_step){browser(message("End of step ", i))}

    if(i != 1)
    {

      check_ll = (log_likelihood_new - log_likelihood_old)

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
  if(browse_at_end){browser()}

  if(i == max_it & !(check_ll < tol_ll & model_coefficient_checks_results)){
    converge = "iterations"
  }


  if(i == 1){
    mu_models_old = NA
    pi_model_old = NA
    log_likelihood_old = NA
    possible_data_old = NA
  }

  return(
    list(
      likelihood = likelihood_documentation %>% as_tibble %>% suppressWarnings() %>% rename(step = V1, likelihood = V2, survreg_maxout = V3) %>% filter(!is.na(likelihood)),
      possible_data = possible_data,
      binom_model = pi_model_new,
      newmodel = mu_model_new,
      steps = i,
      converge = converge,
      ncomp = ncomp,
      prior_step_models = list(mu_model = mu_model_old, pi_model = pi_model_old, log_likelihood = log_likelihood_old, possible_data = possible_data_old)
    )
  )

}



check_mu_model_convergence = function(mu_model, ncomp){
any(is.na(mu_model$scale), is.na(mu_model$coefficients))
}

model_coefficient_checks_safety = function(mu_model_new, pi_model_new, mu_model_old, pi_model_old, model_coefficient_tolerance, ncomp){
  #do the weird coefficients gam returns chaange (only one for s(t) for some reason)
  pi_parametric_coef_check = max((pi_model_new %>% coefficients()) - (pi_model_old %>% coefficients())) < model_coefficient_tolerance

  #these are the actual smoothing things for every observation I believe
  pi_nonparametric_check = max(pi_model_new$smooth - pi_model_old$smooth) < model_coefficient_tolerance

  #check if the number of coefficients in the mu models changes
  mu_number_of_coef_check = (length(na.omit(mu_model_new$coefficients)) == length(na.omit(mu_model_old$coefficients)))

  mu_coefficient_check = ((mu_model_new$coefficients - mu_model_old$coefficients) %>% abs %>% sum) < model_coefficient_tolerance

  mu_sigma_check = ((mu_model_new$scale - mu_model_old$scale) %>% abs) < model_coefficient_tolerance

  #check likelihood at end of E step

  return(all(pi_parametric_coef_check, pi_nonparametric_check, mu_number_of_coef_check, mu_coefficient_check, mu_sigma_check))
}
