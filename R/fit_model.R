#' Title
#'
#' @param visible_data
#' @param max_it
#' @param ncomp
#' @param tol_ll
#'
#' @return
#' @export
#'
fit_model = function(
    visible_data,
    max_it = 3000,
    ncomp = 2,
    tol_ll = 1e-6)
{

  median_y <- median(visible_data$left_bound)
  #first E step-----

  possible_data <-
    visible_data %>% #visible data with c for component
    group_by_all() %>%
    summarise(
      c = as.character(1:2), #split at mean and assign
      `P(C=c|y,t)` = LearnBayes::rdirichlet(1, rep(.1, ncomp)) %>% as.vector(),
      .groups = "drop"
    ) %>%
    # mutate(
    # `P(C=c|y,t)` = case_when(left_bound > median_y & c == "1" ~ 0.6,
    #                          left_bound > median_y & c == "2" ~ 0.4,
    #                          left_bound <= median_y & c == "1" ~ 0.4,
    #                          left_bound <= median_y & c == "2" ~ 0.6)
    # ) %>%
    print()




  likelihood_documentation <- matrix(data = NA, nrow = max_it, ncol = 2)
  likelihood_documentation [,1] <- 1:max_it



  for(i in 1:max_it){
    message("starting iteration number ", i)
    #first M step--------
    #MLE of all parameters
    if(i != 1){
      oldmodel <- newmodel
      oldpi <- pi
      old_log_likelihood <- log_likelihood
      old_possible_data <- possible_data
    }

    #model P(Y|t,c)
    # model0 <- biglm(
    #   formula = y ~ c - 1,
    #   weights =  ~`P(C=c|y,t)`,
    #   data = possible_data) #if not from normal, change the link function and error dist
    #eg if lognormal, survreg with lognormal(change error distand link function)

    model <- survival::survreg(
      formula = Surv(time = left_bound,
                     time2 = right_bound,
                     type = "interval2") ~ 0 + c + strata(c),
      weights = `P(C=c|y,t)`,
      data = possible_data,
      dist = "gaussian")

    newmodel = bind_cols(mean = coef(model), sd = model$scale)

    #Estimate mixing probabilities--------
    pi <- possible_data %>%
      group_by(c) %>%
      summarise(`P(C = c)` = sum(`P(C=c|y,t)`) / nrow(visible_data) )
    #pull(`P(C = c)`, name = c)

    # newmodel <- c(model$coefficients, sigma(model))
    # `sd(y)` = model$scale
    # newmodel = c(coef(model), `sd(y)`)

    if(i != 1){
      check_model = max(abs(newmodel - oldmodel))
      check_pi_tibble = tibble(c = 1:2, pi_dif = pi$`P(C = c)` - oldpi$`P(C = c)`)
      check_pi = max(abs(check_pi_tibble$pi_dif))


      param_checks = check_model < 0.00001 && check_pi < 0.00001


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





    #Next E step-------------
    possible_data %<>%
      select(-any_of("P(C = c)")) %>%
      left_join(pi, by = "c") %>%
      mutate(
        `E[Y|t,c]` = predict(model),
        `sd[Y|t,c]` = model$scale[c],
        `Var[Y|t,c]` = `sd[Y|t,c]`^2,

        `P(Y|t,c)` =  if_else(
          left_bound == right_bound,
          dnorm(x = left_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`),
          pnorm(right_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`) -
            pnorm(left_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`)
        )
      ) %>%
      group_by(obs_id) %>%
      mutate(
        `P(c,y|t)` = `P(Y|t,c)` * `P(C = c)`,
        `P(Y=y|t)` = sum(`P(c,y|t)`),
        `P(C=c|y,t)` = `P(c,y|t)` / `P(Y=y|t)`
      ) %>% ungroup()
    print(pi)
    print(newmodel)

    log_likelihood_obs <- possible_data %>%
      group_by(obs_id) %>%
      summarise(likelihood_i = sum(`P(c,y|t)`)) %>%
      mutate(log_likelihood_i = log(likelihood_i))
    log_likelihood <- sum(log_likelihood_obs$log_likelihood_i)

    likelihood_documentation[i, 2] <- log_likelihood


    message(log_likelihood)

    plot(
      x = likelihood_documentation[1:i,1],
      y = likelihood_documentation[1:i,2],
      type = "l")

    if(i != 1)
    {

      check_ll = log_likelihood - old_log_likelihood

      check_ll_2 = check_ll < tol_ll


      #if(check_ll < 0 ){
      #  warning("Log Likelihood decreased")  ###  HAS BEEN GETTING USED A LOT, WHY IS THE LOG LIKELIHOOD GOING DOWN????
      #}



      if(check_ll_2 & param_checks)
      {
        message("Stopped on combined LL and parameters")
        break
      }

    }



  }

  return(likelihood_documentation[1:i,])

}
