#' fit_model_pi
#'
#' @param visible_data
#' @param formula
#' @param formula2
#' @param max_it
#' @param ncomp
#' @param tol_ll
#' @param browse_at_end
#' @param browse_each_step
#' @param plot_visuals
#' @param prior_step_plot
#' @param pi_link
#' @param verbose
#' @param maxiter_survreg
#' @param initial_weighting
#'
#' @importFrom survival pspline survreg Surv coxph.wtest
#' @importFrom gam gam s lo
#' @importFrom splines ns
#' @importFrom ggplot2 geom_function aes
#'
#' @return
#' @export
#'
#' @examples
fit_model_pi = function(
    visible_data,

    formula = Surv(time = left_bound,
                   time2 = right_bound,
                   type = "interval2") ~ pspline(t, df = 0, calc = TRUE),
    formula2 = c == "2" ~ s(t), #or: c == "2" ~ lo(t)
    max_it = 3000,
    ncomp = 2,
    tol_ll = 1e-6,
    browse_at_end = FALSE,
    browse_each_step = FALSE,
    plot_visuals = FALSE,
    prior_step_plot = FALSE,
    pi_link = "logit",
    #silent = FALSE,
    verbose = 3,
    model_coefficient_tolerance = 0.00001,
    #low_con = 2^-3,
    #high_con = 2^3,
    maxiter_survreg = 30,
    initial_weighting = 8 #smoothingspline or loess
    ){
  #verbose = 0: print nothing
  #verbose = 1: print run number (controlled outside in the purrr::map of this) --done
  #verbose = 2: print run number and iteration number --done
  #verbose = 3: print run number, iteration number, and iteration results --done
  #verbose = 4: print run number, iteration number, iteration results, and run aft as verbose
  #verbose = 0:

  if(ncomp == 1){
    possible_data <-
      visible_data %>%
      mutate(#visible data with c for component
        mid =
          case_when(
            left_bound == -Inf ~ right_bound - 0.5,
            right_bound == Inf ~ left_bound + 0.5,
            TRUE ~ (left_bound + right_bound) / 2
          ),
        rc = ifelse(right_bound == Inf, TRUE, FALSE)
      ) #%>%  ##this is probably only accurate for scale = "log"
    #print()

    newmodel  <- survival::survreg(
      formula,  ##Make this chunk into an argument of the function
      data = possible_data,
      dist = "gaussian",
      control = survreg.control(maxiter = maxiter_survreg, debug = verbose > 3))

    return(list(possible_data = possible_data,
                newmodel = newmodel,
                converge = "YES",
                ncomp = ncomp))

  }else{


  median_y = ifelse(median(visible_data$left_bound) < Inf & median(visible_data$left_bound) > -Inf, median(visible_data$left_bound), mean(c(visible_data$low_con[1], visible_data$high_con[1])))
  #first E step-----
  if(initial_weighting == 1){
    possible_data <-
      visible_data %>% #visible data with c for component
   #   group_by_all() %>%
      reframe(.by = everything(),    #implement for other intial weighting options too ##########
        c = as.character(1:2) #fir a logistic regression on c earlier #########
        # `P(C=c|y,t)` = LearnBayes::rdirichlet(1, rep(.1, ncomp)) %>% as.vector(),
 #       .groups = "drop"
      ) %>%
      #     mutate(
      #     `P(C=c|y,t)` = case_when(left_bound > median_y & c == "1" ~ 0.6,
      #                              left_bound > median_y & c == "2" ~ 0.4,
      #                              left_bound <= median_y & c == "1" ~ 0.4,
      #                              left_bound <= median_y & c == "2" ~ 0.6)
      #     ) %>%
      mutate(
        `P(C=c|y,t)` = case_when(right_bound == Inf & c == "2" ~ 0.9999,
                                 right_bound == Inf & c == "1" ~ 0.0001,
                                 left_bound > median_y & c == "2" ~ ((((left_bound - median_y) / (high_con - median_y)) * 0.5) + 0.5),
                                 left_bound > median_y & c == "1" ~ 1 - ((((left_bound - median_y) / (high_con - median_y)) * 0.5) + 0.5),
                                 left_bound <= median_y & left_bound != -Inf & c == "2" ~ 1 - ((((median_y - left_bound) / (median_y - low_con + 1)) * 0.5) + 0.5),
                                 left_bound <= median_y & left_bound != -Inf & c == "1" ~ ((((median_y - left_bound) / (median_y - low_con + 1)) * 0.5) + 0.5),
                                 left_bound == -Inf & c == "2" ~ 0.0001,
                                 left_bound == -Inf & c == "1" ~ 0.9999),
        mid =
          case_when(
            left_bound == -Inf ~ right_bound - 0.5,
            right_bound == Inf ~ left_bound + 0.5,
            TRUE ~ (left_bound + right_bound) / 2
          ),
        rc = ifelse(right_bound == Inf, TRUE, FALSE)
      ) %>% ungroup() #%>%  ##this is probably only accurate for scale = "log"
    #print()

  } else if(initial_weighting == 2){
    possible_data <-
      visible_data %>% #visible data with c for component
      reframe(.by = everything(),    #implement for other intial weighting options too ##########
              c = as.character(1:2) #fir a logistic regression on c earlier #########
              # `P(C=c|y,t)` = LearnBayes::rdirichlet(1, rep(.1, ncomp)) %>% as.vector(),
              #       .groups = "drop"
      ) %>%

      mutate(
        `P(C=c|y,t)` = case_when(left_bound > median_y & c == "2" ~ (((left_bound - median_y) / (high_con - median_y)) * 0.5) + 0.5 + (0.05 * sample(c(-1, 0, 1), 1)),
                                 left_bound > median_y & c == "1" ~ NaN,
                                 left_bound <= median_y & left_bound != -Inf & c == "2" ~ 1 - ((((median_y - left_bound) / (median_y - low_con + 1)) * 0.5) + 0.5) ,
                                 left_bound <= median_y & left_bound != -Inf & c == "1" ~ NaN,
                                 left_bound == -Inf & c == "2" ~ 0.01,
                                 left_bound == -Inf & c == "1" ~ NaN),
        mid =
          case_when(
            left_bound == -Inf ~ right_bound - 0.5,
            right_bound == Inf ~ left_bound + 0.5,
            TRUE ~ (left_bound + right_bound) / 2
          ),
        rc = ifelse(right_bound == Inf, TRUE, FALSE)
      ) %>% ungroup

    possible_data <- possible_data %>% select(obs_id, `P(C=c|y,t)`) %>% rename(prelim = `P(C=c|y,t)`) %>% filter(!is.na(prelim)) %>% right_join(., possible_data, by = "obs_id") %>% mutate(
      `P(C=c|y,t)` = case_when(
        c == "2" ~ prelim,
        c == "1" ~ 1 - prelim,
        TRUE ~ NaN
      )
    ) %>% select(!prelim)

  }else if(initial_weighting == 3){





    possible_data <-
      visible_data %>% #visible data with c for component
      reframe(.by = everything(),    #implement for other intial weighting options too ##########
              c = as.character(1:2) #fir a logistic regression on c earlier #########
              # `P(C=c|y,t)` = LearnBayes::rdirichlet(1, rep(.1, ncomp)) %>% as.vector(),
              #       .groups = "drop"
      ) %>% #rowwise %>%

      mutate(
        `P(C=c|y,t)` = case_when( left_bound != -Inf & right_bound != Inf & c == "2" ~ .99,
                                  left_bound != -Inf & right_bound != Inf & c == "1" ~ .01,
                                  left_bound == -Inf & c == "2" ~ 0,
                                  left_bound == -Inf & c == "1" ~ 1,
                                  right_bound == Inf & c == "2" ~ 1,
                                  right_bound == Inf & c == "1" ~ 0,
                                  TRUE ~ NaN),
        mid =
          case_when(
            left_bound == -Inf ~ right_bound - 0.5,
            right_bound == Inf ~ left_bound + 0.5,
            TRUE ~ (left_bound + right_bound) / 2
          ),
        rc = ifelse(right_bound == Inf, TRUE, FALSE)
      ) %>% ungroup

    #cases:
    ##left_bound -Inf c ==1, c==2
    ##right_bound Inf, c==1, c==2
    ##else


  } else if(initial_weighting == 4){

    possible_data <-
      reframe(.by = everything(),    #implement for other intial weighting options too ##########
              c = as.character(1:2) #fir a logistic regression on c earlier #########
              # `P(C=c|y,t)` = LearnBayes::rdirichlet(1, rep(.1, ncomp)) %>% as.vector(),
              #       .groups = "drop"
      ) %>%

      mutate(
        `P(C=c|y,t)` = case_when(left_bound > median_y & c == "2" ~ 0.55,
                                 left_bound > median_y & c == "1" ~ 0.45,
                                 left_bound < median_y  & c == "2" ~ 0.45,
                                 left_bound < median_y  & c == "1" ~ 0.55,
                                 left_bound == median_y ~ 0.5,
                                 TRUE ~ NaN),
        mid =
          case_when(
            left_bound == -Inf ~ right_bound - 0.5,
            right_bound == Inf ~ left_bound + 0.5,
            TRUE ~ (left_bound + right_bound) / 2
          ),
        rc = ifelse(right_bound == Inf, TRUE, FALSE)
      ) %>% ungroup

  }else if(initial_weighting == 5){
    possible_data <-
    visible_data %>% #visible data with c for component
      #   group_by_all() %>%
      reframe(.by = everything(),    #implement for other intial weighting options too ##########
              c = as.character(1:2) #fir a logistic regression on c earlier #########
              # `P(C=c|y,t)` = LearnBayes::rdirichlet(1, rep(.1, ncomp)) %>% as.vector(),
              #       .groups = "drop"
      ) %>%
      #     mutate(
      #     `P(C=c|y,t)` = case_when(left_bound > median_y & c == "1" ~ 0.6,
      #                              left_bound > median_y & c == "2" ~ 0.4,
      #                              left_bound <= median_y & c == "1" ~ 0.4,
      #                              left_bound <= median_y & c == "2" ~ 0.6)
      #     ) %>%
      mutate(
        m =  floor(((high_con - low_con) - 1)/ 2),
        `P(C=c|y,t)` = case_when(right_bound == Inf & c == "2" ~ 0.9999,
                                 right_bound == Inf & c == "1" ~ 0.0001,
                                 left_bound == -Inf & c == "2" ~ 0.0001,
                                 left_bound == -Inf & c == "1" ~ 0.9999,
                                 low_con + m >= right_bound & c == "2" ~ 0.1,
                                 low_con + m >= right_bound & c == "1" ~ 0.9,
                                 high_con - m <= left_bound & c == "2" ~ 0.9,
                                 high_con - m <= left_bound & c == "1" ~ 0.1,
                                 TRUE ~ 0.5),
        mid =
          case_when(
            left_bound == -Inf ~ right_bound - 0.5,
            right_bound == Inf ~ left_bound + 0.5,
            TRUE ~ (left_bound + right_bound) / 2
          ),
        rc = ifelse(right_bound == Inf, TRUE, FALSE)
      ) %>% ungroup()
  }else if(initial_weighting == 6){
    possible_data <-
    visible_data %>% #visible data with c for component
      #   group_by_all() %>%
      reframe(.by = everything(),    #implement for other intial weighting options too ##########
              c = as.character(1:2) #fir a logistic regression on c earlier #########
              # `P(C=c|y,t)` = LearnBayes::rdirichlet(1, rep(.1, ncomp)) %>% as.vector(),
              #       .groups = "drop"
      ) %>%
      #     mutate(
      #     `P(C=c|y,t)` = case_when(left_bound > median_y & c == "1" ~ 0.6,
      #                              left_bound > median_y & c == "2" ~ 0.4,
      #                              left_bound <= median_y & c == "1" ~ 0.4,
      #                              left_bound <= median_y & c == "2" ~ 0.6)
      #     ) %>%
      mutate(
        m =  floor(((high_con - low_con) - 1)/ 2),
        mm = floor(((high_con - low_con))/ 2),
        `P(C=c|y,t)` = case_when(right_bound == Inf & c == "2" ~ 0.9999,
                                 right_bound == Inf & c == "1" ~ 0.0001,
                                 left_bound == -Inf & c == "2" ~ 0.0001,
                                 left_bound == -Inf & c == "1" ~ 0.9999,
                                 low_con + m >= right_bound & c == "2" ~ 0.1,
                                 low_con + m >= right_bound & c == "1" ~ 0.9,
                                 high_con - m <= left_bound & c == "2" ~ 0.9,
                                 high_con - m <= left_bound & c == "2" ~ 0.1,
                                 mm > m & low_con + mm >= right_bound & c == "2" ~ 0.3,
                                 mm > m & low_con + mm >= right_bound & c == "1" ~ 0.7,
                                 mm > m & high_con - mm <= left_bound & c == "2" ~ 0.7,
                                 mm > m & high_con - mm <= left_bound & c == "1" ~ 0.3,
                                 TRUE ~ 0.5),
        mid =
          case_when(
            left_bound == -Inf ~ right_bound - 0.5,
            right_bound == Inf ~ left_bound + 0.5,
            TRUE ~ (left_bound + right_bound) / 2
          ),
        rc = ifelse(right_bound == Inf, TRUE, FALSE)
      ) %>% ungroup()
  }else if(initial_weighting == 7){#warningCondition("Select a weight between 1 and 4 please, defaulting to 1")
    possible_data <-
      visible_data %>% #visible data with c for component
      #   group_by_all() %>%
      reframe(.by = everything(),    #implement for other intial weighting options too ##########
              c = as.character(1:2) #fir a logistic regression on c earlier #########
              # `P(C=c|y,t)` = LearnBayes::rdirichlet(1, rep(.1, ncomp)) %>% as.vector(),
              #       .groups = "drop"
      ) %>%
      #     mutate(
      #     `P(C=c|y,t)` = case_when(left_bound > median_y & c == "1" ~ 0.6,
      #                              left_bound > median_y & c == "2" ~ 0.4,
      #                              left_bound <= median_y & c == "1" ~ 0.4,
      #                              left_bound <= median_y & c == "2" ~ 0.6)
      #     ) %>%
      mutate(
        m =  floor(((high_con - low_con) - 1)/ 2),
        mm = floor(((high_con - low_con))/ 2),
        `P(C=c|y,t)` = case_when(right_bound == Inf & c == "2" ~ 1,
                                 right_bound == Inf & c == "1" ~ 0,
                                 left_bound == -Inf & c == "2" ~ 0,
                                 left_bound == -Inf & c == "1" ~ 1,
                                 low_con + m >= right_bound & c == "2" ~ 0.1,
                                 low_con + m >= right_bound & c == "1" ~ 0.9,
                                 high_con - m <= left_bound & c == "2" ~ 0.9,
                                 high_con - m <= left_bound & c == "2" ~ 0.1,
                                 mm > m & low_con + mm >= right_bound & c == "2" ~ 0.1,
                                 mm > m & low_con + mm >= right_bound & c == "1" ~ 0.9,
                                 mm > m & high_con - mm <= left_bound & c == "2" ~ 0.9,
                                 mm > m & high_con - mm <= left_bound & c == "1" ~ 0.1,
                                 TRUE ~ 0.5),
        mid =
          case_when(
            left_bound == -Inf ~ right_bound - 0.5,
            right_bound == Inf ~ left_bound + 0.5,
            TRUE ~ (left_bound + right_bound) / 2
          ),
        rc = ifelse(right_bound == Inf, TRUE, FALSE)
      ) %>% ungroup()
  } else{

  possible_data = initial_weighting_fixed_regression_at_boundaries(visible_data, ncomp)


if(plot_visuals){
    plot_initial_weighting_regression(possible_data)

      browser("stopping at inital setup to examine a plot for the basis of the initial weighting")

}


  }


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
      mu_models_old = mu_models_new
      pi_model_old = pi_model_new
      log_likelihood_old = log_likelihood_new
      possible_data_old = possible_data
    }

   mu_models_new = fit_all_mu_models(possible_data, ncomp, formula, maxiter_survreg)

  likelihood_documentation[i,3] = check_survreg_iteration_maxout(mu_models_new, ncomp, maxiter_survreg)


fit_pi_model = function(pi_formula, pi_link, possible_data){

    if(pi_link == "logit"){
      binom_model = gam::gam(pi_formula, family = binomial(link = "logit"), data = possible_data, weights = `P(C=c|y,t)`)
    } else if(pi_link == "identity"){

      binom_model = gam::gam(pi_formula, family = binomial(link = "identity"), data = possible_data, weights = `P(C=c|y,t)`)
    } else{ errorCondition("pick logit or identity link function")}

  return(binom_model)
}


pi_model_new = fit_pi_model(pi_formula = formula2, pi_link = pi_link, possible_data = possible_data)

check_mu_models_convergence = function(mu_models, ncomp){
  purrr::map(1:ncomp, ~any(is.na(mu_models[[.x]]$scale), is.na(mu_models[[.x]]$coefficients)))
}

if(check_mu_models_convergence(mu_models_new, ncomp) %>% unlist %>% any){
  converge = "NO"
  break
}


    if(i != 1){

      model_coefficient_checks = function(mu_models_new, pi_model_new, mu_models_old, pi_model_old, model_coefficient_tolerance, ncomp){
        #do the weird coefficients gam returns chaange (only one for s(t) for some reason)
        pi_parametric_coef_check = max((pi_model_new %>% coefficients()) - (pi_model_old %>% coefficients())) < model_coefficient_tolerance

        #these are the actual smoothing things for every observation I believe
        pi_nonparametric_check = max(pi_model_new$smooth - pi_model_old$smooth) < model_coefficient_tolerance

        #check if the number of coefficients in the mu models changes
        mu_number_of_coef_check = purrr::map(1:ncomp, ~(length(na.omit(mu_models_new[[.x]]$coefficients)) == length(na.omit(mu_models_old[[.x]]$coefficients)))) %>%
          unlist %>% all

        mu_coefficient_check = purrr::map(1:ncomp, ~((mu_models_new[[.x]]$coefficients - mu_models_old[[.x]]$coefficients) %>% abs %>% sum)) %>% unlist %>% max < model_coefficient_tolerance

        mu_sigma_check = purrr::map(1:ncomp, ~((mu_models_new[[.x]]$scale - mu_models_old[[.x]]$scale) %>% abs)) %>% unlist %>% max < model_coefficient_tolerance

        #check likelihood at end of E step

        return(all(pi_parametric_coef_check, pi_nonparametric_check, mu_number_of_coef_check, mu_coefficient_check, mu_sigma_check))
      }

      model_coefficient_checks_results = model_coefficient_checks(mu_models_new, pi_model_new, mu_models_old, pi_model_old, model_coefficient_tolerance, ncomp)

      }

    #Next E step-------------
possible_data %<>%
  mutate(
    `E[Y|t,c]` = case_when(c == "1" ~ predict(mu_models_new[[1]], newdata = possible_data),
                           c == "2" ~ predict(mu_models_new[[2]], newdata = possible_data),
                           TRUE ~ NaN),
    #predict(model, newdata = possible_data),
    `sd[Y|t,c]` = case_when(c == "1" ~ mu_models_new[[1]]$scale,
                            c == "2" ~ mu_models_new[[2]]$scale, #1,
                            TRUE ~ NaN),
    #model$scale[c], #####QUESTION HERE????????????????????????????
    # `Var[Y|t,c]` = `sd[Y|t,c]`^2,

    `P(Y|t,c)` =  if_else(
      left_bound == right_bound,
      dnorm(x = left_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`),
      pnorm(right_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`) -
        pnorm(left_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`)
    ),
    `P(C=c|t)` = case_when(
      c == "2" ~ predict(binom_model, newdata = tibble(t = t), type = "response"),
      c == "1" ~ 1 - predict(binom_model, newdata = tibble(t = t), type = "response")
    ),
    `P(c,y|t)` = `P(C=c|t)` * `P(Y|t,c)`
  ) %>%
  mutate(.by = obs_id,
         `P(Y=y|t)` = sum(`P(c,y|t)`)) %>%
  mutate(
    `P(C=c|y,t)` = `P(c,y|t)` / `P(Y=y|t)`)
    if(verbose > 2){
      print(pi_model_new)
      print(mu_models_new)
    }
calculate_log_likelihood = function(possible_data){
    log_likelihood_obs <- possible_data %>%
      summarise(.by = obs_id, likelihood_i = sum(`P(c,y|t)`)) %>%
      mutate(log_likelihood_i = log(likelihood_i))
    log_likelihood <- sum(log_likelihood_obs$log_likelihood_i)
    return(log_likelihood)
}

log_likelihood_new = calculate_log_likelihood(possible_data)
    likelihood_documentation[i, 2] <- log_likelihood_new

    if(verbose > 2){
      message(log_likelihood_new)
    }

if(i > 1 && likelihood_documentation[i, 2] < likelihood_documentation[i - 1, 2]){
  browser("likelihood decreased")
}




    if(browse_each_step & plot_visuals){

      browser(message("End of step ", i))
      plot_fm_step(binom_model, newmodel, ncomp, possible_data, prior_step_plot, i)

      if(i > 1){
      plot_likelihood(likelihood_documentation, format = "matrix")
}
    }
    if(browse_each_step & !plot_visuals){browser(message("End of step ", i))}
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

if(i == max_it & !(check_ll < tol_ll & param_checks)){
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
      likelihood = tibble_like(likelihood_documentation),
      possible_data = possible_data,
      binom_model = pi_model_new,
      newmodel = mu_models_new,
      steps = i,
      converge = converge,
      ncomp = ncomp,
      prior_step_models = list(mu_models = mu_models_old, pi_model = pi_model_old, log_likelihood = log_likelihood_old, possible_data = possible_data_old)
    )
  )
  }
}

tibble_like <- function(likelihood_documentation){
  likelihood_documentation %>% as_tibble %>% suppressWarnings() %>% rename(step = V1, likelihood = V2, survreg_maxout = V3) %>% filter(!is.na(likelihood)) %>% return()
}
