EM_algorithm_mgcv = function(
    visible_data,
    mu_formula = yi ~ s(t),
    pi_formula = c == "2" ~ s(t), #or: c == "2" ~ lo(t)
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
    fit_single_component_model(visible_data, mu_formula, maxiter_survreg, verbose) %>% return()

  }else{


    #median_y = ifelse(median(visible_data$left_bound) < Inf & median(visible_data$left_bound) > -Inf, median(visible_data$left_bound), mean(c(visible_data$low_con[1], visible_data$high_con[1])))
    #first E step-----
    if(initial_weighting == 1){
      possible_data = initial_weighting_staggered_weighting_by_distance_from_median_and_boundary(visible_data)
    } else if(initial_weighting == 2){
      possible_data = initial_weighting_staggered_weighting_by_distance_from_median_and_boundary_plus_random_variation(visible_data)
    }else if(initial_weighting == 3){
      possible_data = initial_weighting_flat_interval_censored_full_weight_left_right_censored(visible_data)
    } else if(initial_weighting == 4){
      possible_data = initial_weighting_slight_shift_at_median(visible_data)
    }else if(initial_weighting == 5){
      possible_data = initial_weighting_flat_center_band_of_heavy_weights_at_ends(visible_data)
    }else if(initial_weighting == 6){
      possible_data = initial_weighting_flat_center_two_bands_of_progressively_heavier_weights_at_ends(visible_data)
    } else{

      possible_data = initial_weighting_fixed_regression_at_boundaries(visible_data, ncomp)

      if(plot_visuals){
        plot_initial_weighting_regression(possible_data)

        browser("stopping at inital setup to examine a plot for the basis of the initial weighting")

      }


    }


    likelihood_documentation <- matrix(data = NA, nrow = max_it, ncol = 5)
    likelihood_documentation [,1] <- 1:max_it

    possible_data = possible_data %>% mutate(
      left_bound_mgcv =
        case_when(
          left_bound == -Inf ~ right_bound,
          TRUE ~ left_bound
        ),
      right_bound_mgcv =
        case_when(
          left_bound == -Inf ~ -Inf,
          TRUE ~ right_bound
        )
    )


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

      fit_mgcv_mu_model = function(possible_data, pred_comp, mu_formula, maxiter_survreg = 30){
        df = possible_data %>% filter(`P(C=c|y,t)` > 0 & c == pred_comp)
        df$yi = cbind(df$left_bound_mgcv, df$right_bound_mgcv)
        mgcv::gam(mu_formula, family= mgcv::cnorm(link = "identity"), weights = `P(C=c|y,t)`, data=df) %>% return()
      }

      mu_models_new = #fit_all_mu_models(possible_data, ncomp, formula, maxiter_survreg)
      purrr::map(1:ncomp, ~fit_mgcv_mu_model(possible_data = possible_data, pred_comp = .x, mu_formula = mu_formula, maxiter_survreg = maxiter_survreg))

      fit_mgcv_pi_model = function(pi_formula, pi_link, possible_data){

        if(pi_link == "logit"){
          pi_model = mgcv::gam(pi_formula, family = binomial(link = "logit"), data = possible_data, weights = `P(C=c|y,t)`)
        } else if(pi_link == "identity"){
          pi_model = mgcv::gam(pi_formula, family = binomial(link = "identity"), data = possible_data, weights = `P(C=c|y,t)`)
        }
        if(pi_link == "logit_simple"){
          pi_model = glm(c == "2" ~ t, family = binomial(link = "logit"), data = possible_data, weights = `P(C=c|y,t)`)
        }else{ errorCondition("pick logit or identity link function")}


        return(pi_model)
      }


      pi_model_new = fit_mgcv_pi_model(pi_formula = pi_formula, pi_link = pi_link, possible_data = possible_data)

      check_mu_models_convergence = function(mu_models, ncomp){
        purrr::map(1:ncomp, ~any(is.na(mu_models[[.x]]$family$getTheta(TRUE)), is.na(mu_models[[.x]]$coefficients)))
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
          #pi_nonparametric_check = max(pi_model_new$smooth - pi_model_old$smooth) < model_coefficient_tolerance

          #check if the number of coefficients in the mu models changes
          mu_number_of_coef_check = purrr::map(1:ncomp, ~(length(na.omit(mu_models_new[[.x]]$coefficients)) == length(na.omit(mu_models_old[[.x]]$coefficients)))) %>%
            unlist %>% all

          mu_coefficient_check = purrr::map(1:ncomp, ~((mu_models_new[[.x]]$coefficients - mu_models_old[[.x]]$coefficients) %>% abs %>% sum)) %>% unlist %>% max < model_coefficient_tolerance

          mu_sigma_check = purrr::map(1:ncomp, ~((mu_models_new[[.x]]$family$getTheta(TRUE) - mu_models_old[[.x]]$family$getTheta(TRUE)) %>% abs)) %>% unlist %>% max < model_coefficient_tolerance

          #check likelihood at end of E step

          return(all(pi_parametric_coef_check,
                     #pi_nonparametric_check,
                     mu_number_of_coef_check,
                     mu_coefficient_check,
                     mu_sigma_check))
        }

        model_coefficient_checks_results = model_coefficient_checks(mu_models_new, pi_model_new, mu_models_old, pi_model_old, model_coefficient_tolerance, ncomp)

      }

      #Next E step-------------

      likelihood_documentation[i, 4] <- m_step_check_maximizing(possible_data, mu_models_new, pi_model_new)
      if(i > 1){
        likelihood_documentation[i, 5] <- m_step_check_maximizing(possible_data, mu_models_old, pi_model_old)
      }else{
        likelihood_documentation[i, 5] <- NaN
      }



      possible_data %<>%
        mutate(
          `E[Y|t,c]` = case_when(c == "1" ~ predict(mu_models_new[[1]], newdata = possible_data),
                                 c == "2" ~ predict(mu_models_new[[2]], newdata = possible_data),
                                 TRUE ~ NaN),
          #predict(model, newdata = possible_data),
          `sd[Y|t,c]` = case_when(c == "1" ~ mu_models_new[[1]]$family$getTheta(TRUE),
                                  c == "2" ~ mu_models_new[[2]]$family$getTheta(TRUE), #1,
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
            c == "2" ~ predict(pi_model_new, newdata = tibble(t = t), type = "response"),
            c == "1" ~ 1 - predict(pi_model_new, newdata = tibble(t = t), type = "response")
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
        plot_fm_step(pi_model_new, mu_models_new, ncomp, possible_data, prior_step_plot, i)

        if(i > 1){
          plot_likelihood(likelihood_documentation, format = "matrix")
        }
      }
      if(browse_each_step & !plot_visuals){browser(message("End of step ", i))}
      if(i != 1){

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

EM_algorithm_mgcv(visible_data %>% mutate(obs_id = row_number()))


plot = function(possible_data, mu_models_new, pi_model_new, mu_models_old, pi_model_old){
  mean = possible_data %>% mutate(cens =
                                    case_when(
                                      left_bound == -Inf ~ "lc",
                                      right_bound == Inf ~ "rc",
                                      TRUE ~ "int"
                                    ),
                                  c1_lb = predict(mu_models_new[[1]], tibble(t), se = T)$fit - 1.96 * (predict(mu_models_new[[1]], tibble(t), se = T)$se.fit),
                                  c1_ub = predict(mu_models_new[[1]], tibble(t), se = T)$fit + 1.96 * (predict(mu_models_new[[1]], tibble(t), se = T)$se.fit),
                                  c2_lb = predict(mu_models_new[[2]], tibble(t), se = T)$fit - 1.96 * (predict(mu_models_new[[2]], tibble(t), se = T)$se.fit),
                                  c2_ub = predict(mu_models_new[[2]], tibble(t), se = T)$fit + 1.96 * (predict(mu_models_new[[2]], tibble(t), se = T)$se.fit)) %>%
    ggplot() +
    geom_function(fun = function(t){predict(mu_models_old[[1]], newdata = data.frame(t = t))}, aes(color = "Component 1 Mu Prior", linetype = "Fitted Model")) +
    geom_function(fun = function(t){predict(mu_models_old[[2]], newdata = data.frame(t = t))}, aes(color = "Component 2 Mu Prior", linetype = "Fitted Model")) +
    geom_ribbon(aes(ymin = c1_lb, ymax = c1_ub, x = t, fill = "Component 1 Mu"), alpha = 0.25) +
    geom_function(fun = function(t){predict(mu_models_new[[1]], newdata = data.frame(t = t))}, aes(color = "Component 1 Mu", linetype = "Fitted Model")) +
    geom_function(fun = function(t){predict(mu_models_new[[2]], newdata = data.frame(t = t))}, aes(color = "Component 2 Mu", linetype = "Fitted Model")) +
    geom_ribbon(aes(ymin = c2_lb, ymax = c2_ub, x = t, fill = "Component 2 Mu"), alpha = 0.25) +
    ggnewscale::new_scale_color() +
    scale_color_gradient2(low = "red", high = "blue", mid = "green", midpoint = 0.5) +
    geom_segment(aes(x = t, xend = t, y = left_bound, yend = right_bound, color = `P(C=c|y,t)`), data = (possible_data %>% filter(cens == "int" & c == "2")), alpha = 0.3) +
    geom_segment(aes(x = t, xend = t, y = right_bound, yend = left_bound, color = `P(C=c|y,t)`), data = (possible_data %>% filter(cens == "lc" & c == "2") %>% mutate(left_bound = -5)), arrow = arrow(length = unit(0.03, "npc")), alpha = 0.3) +
    geom_segment(aes(x = t, xend = t, y = left_bound, yend = right_bound, color = `P(C=c|y,t)`), data = (possible_data %>% filter(cens == "rc" & c == "2") %>% mutate(right_bound = 7)), arrow = arrow(length = unit(0.03, "npc")), alpha = 0.3) +
    geom_point(aes(x = t, y = left_bound,  color = `P(C=c|y,t)`), data = possible_data %>% filter(left_bound != -Inf & c == "2"), alpha = 0.3) +
    geom_point(aes(x = t, y = right_bound,  color = `P(C=c|y,t)`), data = possible_data %>% filter(right_bound != Inf & c == "2"), alpha = 0.3) +
    ggtitle(paste0("Iteration ", i, ": Ribbons are the CI for the mean estimates")) +
    xlab("Time") +
    ylab("MIC") +
    ylim(-5, 7)

  pi = ggplot() +
    geom_function(fun = function(t){(1 - predict(pi_model_old, newdata = data.frame(t = t), type = "response"))}, aes(color = "Component 1 Prior")) +
    geom_function(fun = function(t){predict(pi_model_old, newdata = data.frame(t = t), type = "response")}, aes(color = "Component 2 Prior")) +
    geom_function(fun = function(t){(1 - predict(pi_model_new, newdata = data.frame(t = t), type = "response"))}, aes(color = "Component 1")) +
    geom_function(fun = function(t){predict(pi_model_new, newdata = data.frame(t = t), type = "response")}, aes(color = "Component 2")) +
    xlim(0, 16) +
    ylim(0,1)

  return(mean/pi)
}


plot(possible_data, mu_models_new, pi_model_new, mu_models_old, pi_model_old)


load("~/Desktop/scale_demo/example_data")
visible_data = example_data












output = visible_data %>%
  fit_model_mgcv(visible_data = .,
               formula = primary_model_parameters$formula,
               formula2 = primary_model_parameters$formula2,
               max_it = primary_model_parameters$max_it,
               ncomp = 2,
               tol_ll = primary_model_parameters$tol_ll,
               pi_link = primary_model_parameters$pi_link,
               verbose = primary_model_parameters$verbose,
               initial_weighting = primary_model_parameters$initial_weighting,
               browse_each_step = primary_model_parameters$browse_each_step,
               plot_visuals = primary_model_parameters$plot_visuals
  )

title = "enrofl mh mgcv"
add_log_reg = FALSE
plot_fm <- function(output, title, add_log_reg, s_breakpoint, r_breakpoint){

  if(output$ncomp == "2"){
    check_comp_conv = function(models){
      is.na(models$family$getTheta(TRUE)) | (tibble(a = models$coefficients) %>% filter(is.na(a)) %>% nrow) > 0
    }
    results <- tibble(c = 1:2, dnc = purrr::map_lgl(output$newmodel, ~check_comp_conv(.x)))
    if(nrow(results %>% filter(dnc)) > 0){
      fitted_comp = output$newmodel[[results %>% filter(!dnc) %>% pull(c)]]
      ncomp = 1
    } else{
      ncomp = 2
    }
  }else{
    fitted_comp = output$newmodel
    ncomp = 1
  }

  df = output$possible_data %>% mutate(cens =
                                         case_when(
                                           left_bound == -Inf ~ "lc",
                                           right_bound == Inf ~ "rc",
                                           TRUE ~ "int"
                                         ),
                                       mid =
                                         case_when(
                                           left_bound == -Inf ~ right_bound - 0.5,
                                           right_bound == Inf ~ left_bound + 0.5,
                                           TRUE ~ (left_bound + right_bound) / 2
                                         ))

  if(nrow(df %>% filter(left_bound == -Inf)) > 0){
    plot_min_1 <- (df %>% filter(left_bound == -Inf) %>% pull(right_bound) %>% min) - 1
  }else{
    plot_min_1 <- (df %>% pull(left_bound) %>% min) - 1
  }


  if(ncomp == 1){
    plot_min_2 <- sim_pi_survreg_boot(df, fit = fitted_comp, alpha = 0.05, nSims = 10000, distr = "gaussian") %>% pull(lwr) %>% min - 0.2
  } else if(ncomp == 2){
    plot_min_2 <- min(sim_pi_survreg_boot(df %>% filter(c == 1), fit = output$newmodel[[1]], alpha = 0.05, nSims = 10000, distr = "gaussian") %>% pull(lwr) %>% min - 0.2,
                      sim_pi_survreg_boot(df %>% filter(c == 2), fit = output$newmodel[[2]], alpha = 0.05, nSims = 10000, distr = "gaussian") %>% pull(lwr) %>% min - 0.2)
  }else{
    plot_min_2 = plot_min_1
  }

  plot_min = min(plot_min_1, plot_min_2)


  if(nrow(df %>% filter(right_bound == Inf)) > 0){
    plot_max_1 <- (df %>% filter(right_bound == Inf) %>% pull(left_bound) %>% max) + 1
  }else{
    plot_max_1 <- (df %>% pull(right_bound) %>% max) + 1
  }

  if(ncomp == 1){
    plot_max_2 <- sim_pi_survreg_boot(df, fit = fitted_comp, alpha = 0.05, nSims = 10000) %>% pull(upr) %>% max + 0.2
  } else if(ncomp == 2){
    plot_max_2 <- max(sim_pi_survreg_boot(df %>% filter(c ==1), fit = output$newmodel[[1]], alpha = 0.05, nSims = 10000) %>% pull(upr) %>% max + 0.2,
                      sim_pi_survreg_boot(df %>% filter(c == 2), fit = output$newmodel[[2]], alpha = 0.05, nSims = 10000) %>% pull(upr) %>% max + 0.2)
  }else{
    plot_max_2 = plot_max_1
  }

  plot_max = max(plot_max_1, plot_max_2)


  #ciTools::add_pi(df, output$newmodel[[1]], alpha = 0.05, names = c("lwr", "upr"))
  #doesn't work with gaussian dist


  mu.se.brd <- function(t, c, z){predict(output$newmodel[[c]], data.frame(t = t)) + z * predict(output$newmodel[[c]], data.frame(t = t), se = TRUE)$se.fit}
  mu.se.brd.fms <- function(t, z){predict(fitted_comp, data.frame(t = t)) + z * predict(fitted_comp, data.frame(t = t), se = TRUE)$se.fit}

  if(ncomp == 2){

    output$newmodel[[1]]$family$getTheta(TRUE) %>% print
    output$newmodel[[2]]$family$getTheta(TRUE) %>% print

    ci_data <- tibble(t = rep(seq(0, max(output$possible_data$t), len = 300), 2)) %>%
      mutate(
        c1pred = predict(output$newmodel[[1]], tibble(t), se = T)$fit,
        c1pred_se = predict(output$newmodel[[1]], tibble(t), se = T)$se.fit,
        c1pred_lb = c1pred - 1.96 * c1pred_se,
        c1pred_ub = c1pred + 1.96 * c1pred_se,
        c2pred = predict(output$newmodel[[2]], tibble(t), se = T)$fit,
        c2pred_se = predict(output$newmodel[[2]], tibble(t), se = T)$se.fit,
        c2pred_lb = c2pred - 1.96 * c2pred_se,
        c2pred_ub = c2pred + 1.96 * c2pred_se,
      )

    mean <- df %>% ggplot() +
      #geom_bar(aes(x = mid, fill = cens)) +
      geom_function(fun = function(t){predict(output$newmodel[[1]], newdata = data.frame(t = t))}, aes(color = "Component 1 Mu", linetype = "Fitted Model")) +
      geom_function(fun = function(t){predict(output$newmodel[[2]], newdata = data.frame(t = t))}, aes(color = "Component 2 Mu", linetype = "Fitted Model")) +
      geom_ribbon(aes(ymin = c1pred_lb, ymax = c1pred_ub, x = t, fill = "Component 1 Mu"), data = ci_data, alpha = 0.25) +
      geom_ribbon(aes(ymin = c2pred_lb, ymax = c2pred_ub, x = t, fill = "Component 2 Mu"), data = ci_data, alpha = 0.25) +
      geom_ribbon(aes(ymin = lwr, ymax = upr, x = t, fill = "Component 1 Mu"), data = sim_pi_survreg_boot(df, fit = output$newmodel[[1]], alpha = 0.05, nSims = 10000), alpha = 0.15) +
      geom_ribbon(aes(ymin = lwr, ymax = upr, x = t, fill = "Component 2 Mu"), data = sim_pi_survreg_boot(df, fit = output$newmodel[[2]], alpha = 0.05, nSims = 10000), alpha = 0.15) +
      ggnewscale::new_scale_color() +
      scale_colour_gradient2(high = "blue", low = "red", mid = "green", midpoint = 0.5) +
      #geom_point(aes(x = t, y = mid, color = `P(C=c|y,t)`), data = df %>% filter(c == "2"), alpha = 0) +
      geom_segment(aes(x = t, xend = t, y = left_bound, yend = right_bound, color = `P(C=c|y,t)`), data = (df %>% filter(cens == "int" & c == "2")), alpha = 0.3) +
      geom_segment(aes(x = t, xend = t, y = right_bound, yend = left_bound, color = `P(C=c|y,t)`), data = (df %>% filter(cens == "lc" & c == "2") %>% mutate(plot_min)), arrow = arrow(length = unit(0.03, "npc")), alpha = 0.3) +
      geom_segment(aes(x = t, xend = t, y = left_bound, yend = right_bound, color = `P(C=c|y,t)`), data = (df %>% filter(cens == "rc" & c == "2") %>% mutate(plot_max)), arrow = arrow(length = unit(0.03, "npc")), alpha = 0.3) +
      geom_point(aes(x = t, y = left_bound,  color = `P(C=c|y,t)`), data = df %>% filter(left_bound != -Inf & c == "2"), alpha = 0.3) +
      geom_point(aes(x = t, y = right_bound,  color = `P(C=c|y,t)`), data = df %>% filter(right_bound != Inf & c == "2"), alpha = 0.3) +
      #ylim(plot_min - 0.5, plot_max + 0.5) +
      ggtitle(title) +
      xlab("Time") +
      ylab("MIC") +

      #geom_function(fun = function(t){mu.se.brd(t, c = 1, z = 1.96)}, aes(color = "Component 1 Mu", linetype = "Fitted Model SE"), size = 0.6, alpha = 0.6) +
      #geom_function(fun = function(t){mu.se.brd(t, c = 1, z = -1.96)}, aes(color = "Component 1 Mu", linetype = "Fitted Model SE"), size = 0.6, alpha = 0.6) +
      #geom_function(fun = function(t){mu.se.brd(t, c = 2, z = 1.96)}, aes(color = "Component 2 Mu", linetype = "Fitted Model SE"), size = 0.6, alpha = 0.6) +
      #geom_function(fun = function(t){mu.se.brd(t, c = 2, z = -1.96)}, aes(color = "Component 2 Mu", linetype = "Fitted Model SE"), size = 0.6, alpha = 0.6) +
      ylim(plot_min, plot_max)

    ##find sim_pi_survreg_boot in scratch_add_pi_survreg.R

    # need to examine the things for sim_pi_survreg_boot, specifically the vcov stuff and if we should let it draw values for all spline terms and then also for the
    #way it calculates the sim response
    #do we need to account for weighting or anything?

    pi <- ggplot() +
      geom_function(fun = function(t){(1 - predict(output$binom_model, newdata = data.frame(t = t), type = "response"))}, aes(color = "Component 1")) +
      geom_function(fun = function(t){predict(output$binom_model, newdata = data.frame(t = t), type = "response")}, aes(color = "Component 2")) +
      xlim(0, 16) +
      ylim(0,1)
    if(add_log_reg & !is.null(s_breakpoint) & !is.null(r_breakpoint)){
      lr_output = log_reg(output$possible_data, data_type = "possible_data", drug = NULL, date_col = "t", date_type = "decimal", first_year = NULL, s_breakpoint = s_breakpoint, r_breakpoint = r_breakpoint)

      pi = pi +
        geom_function(fun = function(t){(1 - predict(lr_output, newdata = data.frame(t = t), type = "response"))}, aes(color = "Component 1", linetype = "Logistic Regression")) +
        geom_function(fun = function(t){predict(lr_output, newdata = data.frame(t = t), type = "response")}, aes(color = "Component 2", linetype = "Logistic Regression"))
      mean = mean +
        ggnewscale::new_scale_color() +
        geom_hline(aes(yintercept = s_breakpoint %>% parse_number() %>% log2, color = "Susceptible Breakpoint"), alpha = 0.4) +
        geom_hline(aes(yintercept = r_breakpoint %>% parse_number() %>% log2, color = "Resistant Breakpoint"), alpha = 0.4) +
        scale_color_viridis_d(option = "turbo")
    }
    return(mean/pi)

  }else{
    if(output$ncomp == 1){
      ci_data <- tibble(t = rep(seq(0, max(output$possible_data$t), len = 300), 2)) %>%
        mutate(
          c1pred = predict(fitted_comp, tibble(t), se = T)$fit,
          c1pred_se = predict(fitted_comp, tibble(t), se = T)$se.fit,
          c1pred_lb = c1pred - 1.96 * c1pred_se,
          c1pred_ub = c1pred + 1.96 * c1pred_se
        )

      #fitted_comp$scale %>% print()

      mean <- df %>% ggplot() +
        #geom_bar(aes(x = mid, fill = cens)) +
        geom_function(fun = function(t){predict(fitted_comp, newdata = data.frame(t = t))}, aes(color = "Component Mu", linetype = "Fitted Model")) +
        geom_function(fun = function(t){mu.se.brd.fms(t, z = 1.96)}, aes(color = "Component Mu", linetype = "Fitted Model SE"), size = 0.6, alpha = 0.6) +
        geom_function(fun = function(t){mu.se.brd.fms(t, z = -1.96)}, aes(color = "Component Mu", linetype = "Fitted Model SE"), size = 0.6, alpha = 0.6) +
        geom_ribbon(aes(ymin = c1pred_lb, ymax = c1pred_ub, x = t, fill = "Component 1 Mu"), data = ci_data, alpha = 0.2) +
        geom_ribbon(aes(ymin = lwr, ymax = upr, x = t, fill = "Component 1 Mu"), data = sim_pi_survreg_boot(df, fit = fitted_comp, alpha = 0.05, nSims = 10000), alpha = 0.15) +
        ggnewscale::new_scale_color() +
        #geom_point(aes(x = t, y = mid, color = `P(C=c|y,t)`), data = df %>% filter(c == "2"), alpha = 0) +
        geom_segment(aes(x = t, xend = t, y = left_bound, yend = right_bound, color = cens), data = (df %>% filter(cens == "int")), alpha = 0.2) +
        geom_segment(aes(x = t, xend = t, y = right_bound, yend = left_bound, color = cens), data = (df %>% filter(cens == "lc") %>% mutate(left_bound = plot_min)), arrow = arrow(length = unit(0.03, "npc")), alpha = 0.2) +
        geom_segment(aes(x = t, xend = t, y = left_bound, yend = right_bound, color = cens), data = (df %>% filter(cens == "rc") %>% mutate(right_bound = plot_max)), arrow = arrow(length = unit(0.03, "npc")), alpha = 0.2) +
        geom_point(aes(x = t, y = left_bound,  color = cens), data = df %>% filter(left_bound != -Inf), alpha = 0.2) +
        geom_point(aes(x = t, y = right_bound,  color = cens), data = df %>% filter(right_bound != Inf), alpha = 0.2) +
        #scale_colour_gradientn(colours = c("purple", "orange")) +
        #ylim(plot_min - 0.5, plot_max + 0.5) +
        ggtitle(title) +
        xlab("Time") +
        ylab("MIC") +
        #geom_function(fun = function(t){predict(fitted_comp[[2]], newdata = data.frame(t = t))}, aes(color = "Component 2 Mu", linetype = "Fitted Model")) +
        #geom_function(fun = function(t){mu.se.brd(t, c = 1, z = 1.96)}, aes(color = "Component 1 Mu", linetype = "Fitted Model SE"), size = 0.6, alpha = 0.6) +
        #geom_function(fun = function(t){mu.se.brd(t, c = 1, z = -1.96)}, aes(color = "Component 1 Mu", linetype = "Fitted Model SE"), size = 0.6, alpha = 0.6) +
        ylim(plot_min, plot_max)

      return(mean)

    }else{
      ci_data <- tibble(t = rep(seq(0, max(output$possible_data$t), len = 300), 2)) %>%
        mutate(
          c1pred = predict(fitted_comp, tibble(t), se = T)$fit,
          c1pred_se = predict(fitted_comp, tibble(t), se = T)$se.fit,
          c1pred_lb = c1pred - 1.96 * c1pred_se,
          c1pred_ub = c1pred + 1.96 * c1pred_se
        )

      mean <- df %>% ggplot() +
        #geom_bar(aes(x = mid, fill = cens)) +
        geom_function(fun = function(t){predict(fitted_comp, newdata = data.frame(t = t))}, aes(color = "Component Mu", linetype = "Fitted Model")) +
        geom_function(fun = function(t){mu.se.brd.fms(t, z = 1.96)}, aes(color = "Component Mu", linetype = "Fitted Model SE"), size = 0.6, alpha = 0.6) +
        geom_function(fun = function(t){mu.se.brd.fms(t, z = -1.96)}, aes(color = "Component Mu", linetype = "Fitted Model SE"), size = 0.6, alpha = 0.6) +
        geom_ribbon(aes(ymin = c1pred_lb, ymax = c1pred_ub, x = t, fill = "Component 1 Mu"), data = ci_data, alpha = 0.2) +
        geom_ribbon(aes(ymin = lwr, ymax = upr, x = t, fill = "Component 1 Mu"), data = sim_pi_survreg_boot(df, fit = fitted_comp, alpha = 0.05, nSims = 10000), alpha = 0.15) +
        ggnewscale::new_scale_color() +
        scale_colour_gradient2(high = "blue", low = "red", mid = "green", midpoint = 0.5) +
        #geom_point(aes(x = t, y = mid, color = `P(C=c|y,t)`), data = df %>% filter(c == "2"), alpha = 0) +
        geom_segment(aes(x = t, xend = t, y = left_bound, yend = right_bound, color = `P(C=c|y,t)`), data = (df %>% filter(cens == "int" & c == "2")), alpha = 0.2) +
        geom_segment(aes(x = t, xend = t, y = right_bound, yend = left_bound, color = `P(C=c|y,t)`), data = (df %>% filter(cens == "lc" & c == "2") %>% mutate(left_bound = plot_min)), arrow = arrow(length = unit(0.03, "npc")), alpha = 0.2) +
        geom_segment(aes(x = t, xend = t, y = left_bound, yend = right_bound, color = `P(C=c|y,t)`), data = (df %>% filter(cens == "rc"& c == "2") %>% mutate(right_bound = plot_max)), arrow = arrow(length = unit(0.03, "npc")), alpha = 0.2) +
        geom_point(aes(x = t, y = left_bound,  color = `P(C=c|y,t)`), data = df %>% filter(left_bound != -Inf & c == "2"), alpha = 0.2) +
        geom_point(aes(x = t, y = right_bound,  color = `P(C=c|y,t)`), data = df %>% filter(right_bound != Inf & c == "2"), alpha = 0.2) +
        #scale_colour_gradientn(colours = c("purple", "orange")) +
        #ylim(plot_min - 0.5, plot_max + 0.5) +
        ggtitle(title) +
        xlab("Time") +
        ylab("MIC") +
        #geom_function(fun = function(t){predict(fitted_comp[[2]], newdata = data.frame(t = t))}, aes(color = "Component 2 Mu", linetype = "Fitted Model")) +
        #geom_function(fun = function(t){mu.se.brd(t, c = 1, z = 1.96)}, aes(color = "Component 1 Mu", linetype = "Fitted Model SE"), size = 0.6, alpha = 0.6) +
        #geom_function(fun = function(t){mu.se.brd(t, c = 1, z = -1.96)}, aes(color = "Component 1 Mu", linetype = "Fitted Model SE"), size = 0.6, alpha = 0.6) +
        ylim(plot_min, plot_max)

      pi <- ggplot() +
        geom_function(fun = function(t){(1 - predict(output$binom_model, newdata = data.frame(t = t), type = "response"))}, aes(color = "Component 1", linetype = "Model")) +
        geom_function(fun = function(t){predict(output$binom_model, newdata = data.frame(t = t), type = "response")}, aes(color = "Component 2", linetype = "Model")) +
        xlim(0, 16) +
        ylim(0,1)


      if(add_log_reg & !is.null(s_breakpoint) & !is.null(r_breakpoint)){
        lr_output = log_reg(output$possible_data, data_type = "possible_data", drug = NULL, date_col = "t", date_type = "decimal", first_year = NULL, s_breakpoint = s_breakpoint, r_breakpoint = r_breakpoint)

        pi = pi +
          geom_function(fun = function(t){(1 - predict(lr_output, newdata = data.frame(t = t), type = "response"))}, aes(color = "Component 1", linetype = "Logistic Regression")) +
          geom_function(fun = function(t){predict(lr_output, newdata = data.frame(t = t), type = "response")}, aes(color = "Component 2", linetype = "Logistic Regression"))
        mean = mean +
          ggnewscale::new_scale_color() +
          geom_hline(aes(yintercept = s_breakpoint %>% parse_number() %>% log2, color = "Susceptible Breakpoint"), alpha = 0.4) +
          geom_hline(aes(yintercept = r_breakpoint %>% parse_number() %>% log2, color = "Resistant Breakpoint"), alpha = 0.4) +
          scale_color_viridis_d(option = "turbo")
      }
      return(mean/pi)
    }
  }


  ##maxing out iterations fails to generate a `converge` object!!!!!!!!!!!!!!

}
