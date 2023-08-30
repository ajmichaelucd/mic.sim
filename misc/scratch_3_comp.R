fit_model_pi_3 = function(
    visible_data,

    formula = Surv(time = left_bound,
                   time2 = right_bound,
                   type = "interval2") ~ pspline(t, df = 0, calc = TRUE),
    formula3 = list(cc ~ s(t), ~ s(t)), #or: c == "2" ~ lo(t)
    max_it = 3000,
    ncomp = 2,
    tol_ll = 1e-6,
    browse_at_end = FALSE,
    browse_each_step = FALSE,
    plot_visuals = FALSE,
    pi_link = "logit",
    #silent = FALSE,
    verbose = 3,
    #low_con = 2^-3,
    #high_con = 2^3,
    maxiter_survreg = 30,
    initial_weighting = 1 #smoothingspline or loess
){
  #verbose = 0: print nothing
  #verbose = 1: print run number (controlled outside in the purrr::map of this) --done
  #verbose = 2: print run number and iteration number --done
  #verbose = 3: print run number, iteration number, and iteration results --done
  #verbose = 4: print run number, iteration number, iteration results, and run aft as verbose
  #verbose = 0:


  mean_width <- visible_data %>% mutate(diff = high_con - low_con) %>% summarise(mean = mean(diff)) %>% pull

  if(mean_width <= 3){##put everything in here, this is because without at least 3 intervals we can't detect a third comp
  return(
    "Interval too narrow"
  )
    }

  #first E step-----
if(initial_weighting == 1){
    possible_data <-
      visible_data %>% #visible data with c for component
      #   group_by_all() %>%
      reframe(.by = everything(),    #implement for other intial weighting options too ##########
              c = as.character(1:3) #fir a logistic regression on c earlier #########
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
        `P(C=c|y,t)` = case_when(right_bound == Inf & c == "3" ~ 0.99,
                                 right_bound == Inf & c == "2" ~ 0.01,
                                 right_bound == Inf & c == "1" ~ 0,
                                 #left_bound > median_y & c == "2" ~ ((((left_bound - median_y) / (high_con - median_y)) * 0.5) + 0.5),
                                 #left_bound > median_y & c == "1" ~ 1 - ((((left_bound - median_y) / (high_con - median_y)) * 0.5) + 0.5),
                                 #left_bound <= median_y & left_bound != -Inf & c == "2" ~ 1 - ((((median_y - left_bound) / (median_y - low_con + 1)) * 0.5) + 0.5),
                                 #left_bound <= median_y & left_bound != -Inf & c == "1" ~ ((((median_y - left_bound) / (median_y - low_con + 1)) * 0.5) + 0.5),
                                 left_bound == -Inf & c == "3" ~ 0,
                                 left_bound == -Inf & c == "2" ~ 0.01,
                                 left_bound == -Inf & c == "1" ~ 0.99,
                                 right_bound == low_con & c == "3" ~ 0.001,
                                 right_bound == low_con & c == "2" ~ 0.499,
                                 right_bound == low_con & c == "1" ~ 0.5,
                                 left_bound == high_con & c == "3" ~ 0.5,
                                 left_bound == high_con & c == "2" ~ 0.499,
                                 left_bound == high_con & c == "1" ~ 0.001,
                                 TRUE ~ (1/3)),
        mid =
          case_when(
            left_bound == -Inf ~ right_bound - 0.5,
            right_bound == Inf ~ left_bound + 0.5,
            TRUE ~ (left_bound + right_bound) / 2
          ),
        rc = ifelse(right_bound == Inf, TRUE, FALSE)
      ) %>% ungroup()

} else{
  visible_data <- visible_data %>% mutate(cens = case_when(
    left_bound == -Inf | right_bound == low_con ~ "lc",
    right_bound == Inf |
      left_bound == high_con ~ "rc",
    TRUE ~ "int"
  ))
  n_obs <- nrow(visible_data)

  full_set = tibble(
    cens = c("rc", "lc", "int")
  )

  cens_counts <-
    visible_data %>%
    summarize(.by = cens,
              n = n()
    ) %>% right_join(., full_set) %>% mutate(n = case_when(
      is.na(n) ~ 0,
      TRUE ~ n
    )) %>% pivot_wider(
      names_from = cens, values_from = n
    )

  lc <- cens_counts %>% pull(lc)
  int <- cens_counts %>% pull(int) #implement for 2 comp too
  rc <- cens_counts %>% pull(rc)
  `P(1)` <- (lc + 1) / (n_obs + 3)
  `P(2)` <- (int + 1) / (n_obs + 3)
  `P(3)` <- (rc + 1) / (n_obs + 3)
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
      oldmodel <- newmodel
      oldpi <- pi
      old_log_likelihood <- log_likelihood
      old_possible_data <- possible_data
      oldbinommodel <- newbinommodel
    }




    #model P(Y|t,c)
    # model0 <- biglm(
    #   formula = y ~ c - 1,
    #   weights =  ~`P(C=c|y,t)`,
    #   data = possible_data) #if not from normal, change the link function and error dist
    #eg if lognormal, survreg with lognormal(change error distand link function)

    df_temp <- possible_data %>% filter(`P(C=c|y,t)` != 0 )
    #possible_data <- possible_data %>% filter(`P(C=c|y,t)` != 0 )




    #print("about to survreg")

    df_temp %>% filter(c == 1) -> df1
    df_temp %>% filter(c == 2) -> df2
    df_temp %>% filter(c == 2) -> df3
    modelsplit_1 <- survival::survreg(
      formula,  ##Make this chunk into an argument of the function
      weights = `P(C=c|y,t)`,
      data = df1,
      dist = "gaussian",
      control = survreg.control(maxiter = maxiter_survreg, debug = verbose > 3))
    modelsplit_2 <- survival::survreg(
      formula,  ##Make this chunk into an argument of the function
      weights = `P(C=c|y,t)`,
      data = df2,
      dist = "gaussian",
      control = survreg.control(maxiter = maxiter_survreg, debug = verbose > 3))
    modelsplit_3 <- survival::survreg(
      formula,  ##Make this chunk into an argument of the function
      weights = `P(C=c|y,t)`,
      data = df3,
      dist = "gaussian",
      control = survreg.control(maxiter = maxiter_survreg, debug = verbose > 3))
    #
    #model <- survival::survreg(
    #  formula,  ##Make this chunk into an argument of the function
    #  weights = `P(C=c|y,t)`,
    #  data = df_temp,
    #  dist = "gaussian",
    #  control = survreg.control(maxiter = maxiter_survreg, debug = verbose > 3))
    #summary(modelsplit_1)
    #summary(modelsplit_2)

    if (modelsplit_1$iter[1] == maxiter_survreg | modelsplit_2$iter[1] == maxiter_survreg | modelsplit_3$iter[1] == maxiter_survreg){
      likelihood_documentation[i,3] <- TRUE
    } else{
      likelihood_documentation[i,3] <- FALSE
    }
    #browser()

    #   if(plot_visuals){
    #     zz <- seq(0, max(visible_data$t), len = 300)
    #     preds <- predict(binom_model, newdata = data.frame(t = zz), se = TRUE)
    #
    #
    #     plot(binom_model, se = T, col = "green")
    #
    #
    #     preds
    #     tibble(t = zz, spline.predict = predict(binom_model, data.frame(t = zz)))
    #     ggplot() %>%
    #       geom_smooth()
    #   }



    newmodel = list(modelsplit_1, modelsplit_2, modelsplit_3)

    nn <- paste("c", 1:ncomp, sep = "")

    ##newmodel2 <- tibble(t(data.frame(split(newmodel$mean, rep_len(nn, length.out = length(newmodel$mean)))))) %>%
    # rename(comp_mean = 1) %>%
    # mutate(sd = c(newmodel$sd)) %>%
    # mutate(names = nn) %>%
    # relocate(names, .before = everything()) %>% print()

    #Estimate mixing probabilities--------
    #   pi <- possible_data %>%
    #     group_by(c) %>%
    #     summarise(`P(C = c)` = sum(`P(C=c|y,t)`) / nrow(visible_data) )
    #   #pull(`P(C = c)`, name = c)

    #pi = newbinommodel

    #   pi <- possible_data  %>% mutate(`P(C=c|y,t)` = case_when(
    #     c == "2" ~ predict(logit, newdata = tibble(t = possible_data$t), type = "response"),
    #     c == "1" ~ 1 - predict(logit, newdata = tibble(t = possible_data$t), type = "response")
    #   ))
    #   if(pi_link == "logit"){
    #     binom_model <- stats::glm(formula2, family = binomial(link = "logit"), data = possible_data, weights = `P(C=c|y,t)`)
    #   } else if(pi_link == "identity"){
    #
    #     binom_model <- stats::glm(formula2, family = binomial(link = "identity"), data = possible_data, weights = `P(C=c|y,t)`)
    #   } else{ errorCondition("pick logit or identity link function")}

 #   library(nnet)
    possible_data <- possible_data %>% mutate(cc = case_when(
      c == "1" ~ 0,
      c == "2" ~ 1,
      TRUE ~ 2
    ))
#    possible_data$cc <- relevel(as.factor(possible_data$cc), ref = 1)

 #   multinom(formula = formula3, data = possible_data, model = TRUE, weights = `P(C=c|y,t)`)

#    library(mgcv)
#    t = possible_data$t
#    y = possible_data$cc
    rrr <- mgcv::gam(data = possible_data %>% filter( `P(C=c|y,t)` > 0), formula = formula3, family = mgcv::multinom(K = 2), weights = `P(C=c|y,t)`)

    #mgcv::predict.gam(rrr, newdata = data.frame(t = seq(0:15)), type="response")

 #   detach("package:mgcv", unload = TRUE)



#
  #  if(pi_link == "logit"){
  #    binom_model <- gam::gam(formula2, family = binomial(link = "logit"), data = possible_data, weights = `P(C=c|y,t)`)
  #  } else if(pi_link == "identity"){
#
  #    binom_model <- gam::gam(formula2, family = binomial(link = "identity"), data = possible_data, weights = `P(C=c|y,t)`)
  #  } else{ errorCondition("pick logit or identity link function")}


    #    if(plot_visuals = TRUE){
    #      zz <- seq(0, max(visible_data$t), len = 300)
    #      predict(binom_model, data.frame(t = zz))
    #
    #      gam::predict_gam(binom_model) %>% ggplot() %>%
    #        geom_smooth(aes(t, fit))
    #    }



    newbinommodel <- binom_model

    pi = newbinommodel
    # newmodel <- c(model$coefficients, sigma(model))
    # `sd(y)` = model$scale
    # newmodel = c(coef(model), `sd(y)`)

    if(i != 1){
      # check_model = max(abs(newmodel - oldmodel))
      # check_pi_tibble = tibble(c = 1:2, pi_dif = pi$`P(C = c)` - oldpi$`P(C = c)`)
      #check_pi = max(abs(newbinommodel - oldbinommodel))
      #  check_pi = max(abs(check_pi_tibble$pi_dif))

      number_coef_1 <- length(modelsplit_1$coefficients) == length(oldmodel[[1]]$coefficients)
      number_coef_2 <- length(modelsplit_2$coefficients) == length(oldmodel[[2]]$coefficients)


      if(number_coef_1 & number_coef_2){
        mu_coef_diff_1 <-  sum(abs(modelsplit_1$coefficients - oldmodel[[1]]$coefficients))
        mu_coef_diff_2 <-  sum(abs(modelsplit_2$coefficients - oldmodel[[2]]$coefficients))
        mu_coef_diff <- max(mu_coef_diff_1, mu_coef_diff_2) < 0.00001
        #binom_model$smooth.frame ????

      } else{
        mu_coef_diff <- FALSE
      }

      if(is.na(mu_coef_diff)){
        converge = "NO"
        break

      }

      #binom_model$coefficients

      #pi_coef_diff <- length(modelsplit_2$coefficients) == length(oldmodel[[2]]$coefficients)

      #summary(binom_model)
      param_checks = mu_coef_diff #&& check_pi < 0.00001


      if( mu_coef_diff ) # && check_pi < 0.00001)
      {message("stopped on coefficients")
        converge = "YES"
        break}
      #
    }
    #marginal_likelihood  #add warning: Chicago style pizza is a casserole
    #observed data likelihood sum(log(P(y_i|t_i)))
    #`P(Y|t,c)` * `P(C=c|t)` = P(Y, C|t) then sum over possible values of C
    #aka P(C = c)

    #group by obs_id then sum `P(c,y|t)` to get likelihood for each observation, take log of that, and then sum of those is observed data likelihood


    #once we have likelihood, make sure:
    #A. it is going up (with EM algorithm it should always increase)
    #B. if it is not going up by very much, you can stop
    #if conditions are met, use break
    if(plot_visuals == TRUE){
      ##outdated
      c1_plot <-   possible_data %>%
        filter(c == 1) %>%
        ggplot(mapping = aes(x = t, y = mid, color = `P(C=c|y,t)`)) +
        geom_point() +
        geom_abline(data = NULL, intercept = newmodel$mean[,"c1"], slope = newmodel$mean[,"c1:t"], color = "red") +
        geom_abline(data = NULL, intercept = newmodel$mean[,"c2"], slope = newmodel$mean[,"c2:t"], color = "violet")+
        expand_limits(y = c(newmodel$mean[,"c1"], newmodel$mean[,"c2"]))
      print(c1_plot)
    }

    #Next E step-------------
    possible_data %<>%
      #     select(-any_of("P(C = c)")) %>%
      #      left_join(pi, by = "c") %>%
      mutate(
        `E[Y|t,c]` = case_when(c == "1" ~ predict(modelsplit_1, newdata = possible_data),
                               c == "2" ~ predict(modelsplit_2, newdata = possible_data),
                               c == "3" ~ predict(modelsplit_3, newdata = possible_data),
                               TRUE ~ NaN),
        #predict(model, newdata = possible_data),
        `sd[Y|t,c]` = case_when(c == "1" ~ modelsplit_1$scale,
                                c == "2" ~ modelsplit_2$scale,
                                c == "3" ~ modelsplit_3$scale,
                                TRUE ~ NaN),
        #model$scale[c], #####QUESTION HERE????????????????????????????
        # `Var[Y|t,c]` = `sd[Y|t,c]`^2,

        `P(Y|t,c)` =  if_else(
          left_bound == right_bound,
          dnorm(x = left_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`),
          pnorm(right_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`) -
            pnorm(left_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`)
        )
      ) %>%
      group_by(obs_id) %>% #rowwise %>%
      #  mutate(
      #    `P(c,y|t)` = `P(Y|t,c)` * `P(C = c)`, #unsure here
      #    `P(Y=y|t)` = sum(`P(c,y|t)`),
      #    `P(C=c|y,t)` = `P(c,y|t)` / `P(Y=y|t)`
      #  )
      mutate(`P(C=c|t)` = case_when( ########UNSURE ABOUT THIS SECTION
        c == "3" ~ mgcv::predict.gam(rrr, newdata = tibble(t = t), type="response")[,3],
        c == "2" ~ mgcv::predict.gam(rrr, newdata = tibble(t = t), type="response")[,2], ########UNSURE ABOUT THIS SECTION
        c == "1" ~ mgcv::predict.gam(rrr, newdata = tibble(t = t), type="response")[,1] ########UNSURE ABOUT THIS SECTION
      )) %>%  ########UNSURE ABOUT THIS SECTION
      mutate(`P(c,y|t)` = `P(C=c|t)` * `P(Y|t,c)`, ########UNSURE ABOUT THIS SECTION
             `P(Y=y|t)` = sum(`P(c,y|t)`), ########UNSURE ABOUT THIS SECTION
             `P(C=c|y,t)` = `P(c,y|t)` / `P(Y=y|t)`) %>%  ########UNSURE ABOUT THIS SECTION
      ungroup()
    if(verbose > 2){
      print(pi)
      print(newmodel)
      #add print here
    }

    log_likelihood_obs <- possible_data %>%
      group_by(obs_id) %>%
      summarise(likelihood_i = sum(`P(c,y|t)`)) %>%
      mutate(log_likelihood_i = log(likelihood_i))
    log_likelihood <- sum(log_likelihood_obs$log_likelihood_i)

    likelihood_documentation[i, 2] <- log_likelihood

    if(verbose > 2){
      message(log_likelihood)
    }
    #par(mfrow = c(2,1))
    #    plot(
    #      x = likelihood_documentation[1:i,1],
    #      y = likelihood_documentation[1:i,2],
    #      type = "l")







    if(browse_each_step){browser(message("End of step ", i))}
    if(i != 1)
    {

      check_ll = log_likelihood - old_log_likelihood

      check_ll_2 = check_ll < tol_ll

      #    other_plot <- tibble(likelihood_documentation) %>% ggplot() +
      #      geom_line(aes(x = likelihood_documentation[,1],
      #                    y = likelihood_documentation[,2]))
      #
      #    print(grid.arrange(c1_plot, other_plot, nrow = 1))

      #if(check_ll < 0 ){
      #  warning("Log Likelihood decreased")  ###  HAS BEEN GETTING USED A LOT, WHY IS THE LOG LIKELIHOOD GOING DOWN????
      #}



      if(check_ll_2 & param_checks)
      {
        if(verbose > 0){
          message("Stopped on combined LL and parameters")}
        converge = "YES"
        break
      }

    }



  }
  if(browse_at_end){browser()}



  return(
    list(
      likelihood = likelihood_documentation[1:i, ],
      possible_data = possible_data,
      binom_model = binom_model,
      newmodel = newmodel,
      steps = i,
      converge = converge
    )
  )

}
