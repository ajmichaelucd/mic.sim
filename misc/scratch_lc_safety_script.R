fit_model_safety_pi = function(
    visible_data = prep_sim_data_for_em(),
    formula = Surv(time = left_bound,
                   time2 = right_bound,
                   type = "interval2") ~ pspline(t, df = 0, calc = TRUE),
    formula2 = c == "2" ~ s(t),
    fm_check = "RC",
    max_it = 3000,
    ncomp = 2,
    tol_ll = 1e-6,
    browse_at_end = FALSE,
    browse_each_step = FALSE,
    plot_visuals = FALSE,
    pi_link = "logit",
    verbose = 3,
    maxiter_survreg = 30){
  #verbose = 0: print nothing
  #verbose = 1: print run number (controlled outside in the purrr::map of this) --done
  #verbose = 2: print run number and iteration number --done
  #verbose = 3: print run number, iteration number, and iteration results --done
  #verbose = 4: print run number, iteration number, iteration results, and run aft as verbose
  #verbose = 0:



  median_y = median(visible_data$left_bound)
  #first E step-----
possible_data = case_when(
  fm_check == "RC" ~
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
    ), #%>%

  fm_check == "LC" ~
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
    ),
  TRUE ~ "Error"
)

if(possible_data = "Error"){errorCondition("invalid fm_check value")}


  likelihood_documentation <- matrix(data = NA, nrow = max_it, ncol = 3) ##changed to 3 to log survreg warnings
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

    df_temp <- case_when(fm_check == "RC" ~ possible_data %>% filter(`P(C=c|y,t)` != 0 , c == 1),
                         fm_check == "LC" ~ possible_data %>% filter(`P(C=c|y,t)` != 0 , c == 2),
                         TRUE ~ possible_data %>% filter(`P(C=c|y,t)` != 0 , c == 1)
    )


    #possible_data <- possible_data %>% filter(`P(C=c|y,t)` != 0 )
    #print("about to survreg")
    model <- survival::survreg(
      formula,  ##Make this chunk into an argument of the function
      weights = `P(C=c|y,t)`,
      data = df_temp,
      dist = "gaussian",
      control = survreg.control(maxiter = maxiter_survreg, debug = verbose > 3))


    if (model$iter[1] == maxiter_survreg){
      likelihood_documentation[i,3] <- TRUE
    } else{
      likelihood_documentation[i,3] <- FALSE
    }
    #browser()




    newmodel = model

    nn <- paste("c", 1:ncomp, sep = "")

    ##newmodel2 <- tibble(t(data.frame(split(newmodel$mean, rep_len(nn, length.out = length(newmodel$mean)))))) %>%
    # rename(comp_mean = 1) %>%
    # mutate(sd = c(newmodel$sd)) %>%
    # mutate(names = nn) %>%
    # relocate(names, .before = everything()) %>% print()

    #Estimate mixing probabilities--------
    #pi = newbinommodel

    #   pi <- possible_data  %>% mutate(`P(C=c|y,t)` = case_when(
    #     c == "2" ~ predict(logit, newdata = tibble(t = possible_data$t), type = "response"),
    #     c == "1" ~ 1 - predict(logit, newdata = tibble(t = possible_data$t), type = "response")
    #   ))
    if(pi_link == "logit"){
      #   binom_model <- stats::glm(formula2, family = binomial(link = "logit"), data = possible_data, weights = `P(C=c|y,t)`)
      binom_model <- gam::gam(formula2, family = binomial(link = "logit"), data = possible_data, weights = `P(C=c|y,t)`)
    } else if(pi_link == "identity"){

      binom_model <- gam::gam(formula2, family = binomial(link = "identity"), data = possible_data, weights = `P(C=c|y,t)`)
    } else{ errorCondition("pick logit or identity link function")}

    newbinommodel <- binom_model

    pi = newbinommodel
    #pull(`P(C = c)`, name = c)

    # newmodel <- c(model$coefficients, sigma(model))
    # `sd(y)` = model$scale
    # newmodel = c(coef(model), `sd(y)`)

    if(i != 1){


      number_coef <- length(model$coefficients) == length(oldmodel$coefficients)
      if(number_coef){
        mu_coef_diff <-  max(abs(model$coefficients - oldmodel$coefficients)) < 0.00001

      } else{
        mu_coef_diff <- FALSE
      }

      param_checks = mu_coef_diff #&& check_pi < 0.00001


      if( mu_coef_diff ) # && check_pi < 0.00001)
      {message("stopped on coefficients")
        break}
      # check_model = max(abs(newmodel - oldmodel))
      # check_pi_tibble = tibble(c = 1:2, pi_dif = pi$`P(C = c)` - oldpi$`P(C = c)`)
      # check_pi = max(abs(check_pi_tibble$pi_dif))
      #
      #
      # param_checks = check_model < 0.00001 && check_pi < 0.00001
      #

      #  if( check_model < 0.00001 && check_pi < 0.00001)
      #  {message("stopped on coefficients")
      #    break}
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
        mutate(
          mid =
            case_when(
              left_bound == -Inf ~ right_bound - 0.5,
              right_bound == Inf ~ left_bound + 0.5,
              TRUE ~ (left_bound + right_bound) / 2
            )
        ) %>%
        filter(c == 1) %>%
        ggplot(mapping = aes(x = t, y = mid, color = `P(C=c|y,t)`)) +
        geom_point() +
        geom_abline(data = NULL, intercept = newmodel$mean[,"c1"], slope = newmodel$mean[,"c1:t"], mapping = aes(col = "c1")) +
        geom_abline(data = NULL, intercept = newmodel$mean[,"c2"], slope = newmodel$mean[,"c2:t"], mapping = aes(col = "c2"))
      print(c1_plot)
    }

    #Next E step-------------
    possible_data %<>%
      #select(-any_of("P(C = c)")) %>%
      #left_join(pi, by = "c") %>%
      mutate(
        `E[Y|t,c]` = if_else( c==1, predict(model, newdata = possible_data), NA_real_),
        `sd[Y|t,c]` = if_else( c==1, model$scale, NA_real_),
        # `Var[Y|t,c]` = `sd[Y|t,c]`^2,

        `P(Y|t,c)` =  if_else(c== 1,
                              if_else(
                                left_bound == right_bound,
                                dnorm(x = left_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`),
                                pnorm(right_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`) -
                                  pnorm(left_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`)
                              ), (right_bound == Inf) %>% as.numeric()) ##possibly add upper concentation as input and change this to right_bound == Inf & left_bound == high_con
      ) %>%
      group_by(obs_id) %>%
      #mutate(
      #  `P(c,y|t)` = `P(Y|t,c)` * `P(C = c)`,
      #  `P(Y=y|t)` = sum(`P(c,y|t)`),
      #  `P(C=c|y,t)` = `P(c,y|t)` / `P(Y=y|t)`
      #) %>%
      mutate(`P(C=c|t)` = case_when( ########UNSURE ABOUT THIS SECTION
        c == "2" ~ predict(binom_model, newdata = tibble(t = t), type = "response"), ########UNSURE ABOUT THIS SECTION
        c == "1" ~ 1 - predict(binom_model, newdata = tibble(t = t), type = "response") ########UNSURE ABOUT THIS SECTION
      )) %>%  ########UNSURE ABOUT THIS SECTION
      mutate(`P(c,y|t)` = `P(C=c|t)` * `P(Y|t,c)`, ########UNSURE ABOUT THIS SECTION
             `P(Y=y|t)` = sum(`P(c,y|t)`), ########UNSURE ABOUT THIS SECTION
             `P(C=c|y,t)` = `P(c,y|t)` / `P(Y=y|t)`) %>%
      ungroup()
    if(verbose > 2){
      print(pi)
      print(newmodel)
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
      steps = i
    )
  )

}
