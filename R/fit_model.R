#' Title
#'
#' @param visible_data
#' @param max_it
#' @param ncomp
#' @param tol_ll
#' @param formula
#' @param browse_at_end
#' @param browse_each_step
#' @param plot_visuals
#' @param verbose
#' @param maxiter_survreg
#'
#' @importFrom gridExtra grid.arrange
#' @importFrom ggplot2 ggplot geom_point
#' @importFrom survival strata survreg Surv survreg.control
#' @importFrom pryr mem_used
#'
#' @return
#' @export
#'
fit_model = function(
    visible_data,
    formula = Surv(time = left_bound,
                   time2 = right_bound,
                   type = "interval2") ~ 0 + c + strata(c) + t:c,
    max_it = 3000,
    ncomp = 2,
    tol_ll = 1e-6,
    browse_at_end = FALSE,
    browse_each_step = FALSE,
    plot_visuals = FALSE,
    #silent = FALSE,
    verbose = 3,
    #low_con = 2^-3,
    #high_con = 2^3,
    maxiter_survreg = 30)
#verbose = 0: print nothing
#verbose = 1: print run number (controlled outside in the purrr::map of this) --done
#verbose = 2: print run number and iteration number --done
#verbose = 3: print run number, iteration number, and iteration results --done
#verbose = 4: print run number, iteration number, iteration results, and run aft as verbose
#verbose = 0:

{

  median_y = median(visible_data$left_bound)
  #first E step-----

  possible_data <-
    visible_data %>% #visible data with c for component
    group_by_all() %>%
    summarise(
      c = as.character(1:2), #split at mean and assign
     # `P(C=c|y,t)` = LearnBayes::rdirichlet(1, rep(.1, ncomp)) %>% as.vector(),
      .groups = "drop"
    ) %>%
#     mutate(
#     `P(C=c|y,t)` = case_when(left_bound > median_y & c == "1" ~ 0.6,
#                              left_bound > median_y & c == "2" ~ 0.4,
#                              left_bound <= median_y & c == "1" ~ 0.4,
#                              left_bound <= median_y & c == "2" ~ 0.6)
#     ) %>%
    mutate(
      `P(C=c|y,t)` = case_when(left_bound > median_y & c == "1" ~ (((left_bound - median_y) / (high_con - median_y)) * 0.5) + 0.5,
                               left_bound > median_y & c == "2" ~ 1 - ((((left_bound - median_y) / (high_con - median_y)) * 0.5) + 0.5),
                               left_bound <= median_y & left_bound != -Inf & c == "1" ~ 1 - ((((median_y - left_bound) / (median_y - low_con + 1)) * 0.5) + 0.5),
                               left_bound <= median_y & left_bound != -Inf & c == "2" ~ (((median_y - left_bound) / (median_y - low_con + 1)) * 0.5) + 0.5,
                               left_bound == -Inf & c == "1" ~ 0.01,
                               left_bound == -Inf & c == "2" ~ 0.99),
          mid =
            case_when(
              left_bound == -Inf ~ right_bound - 0.5,
              right_bound == Inf ~ left_bound + 0.5,
              TRUE ~ (left_bound + right_bound) / 2
            ),
      rc = ifelse(right_bound == Inf, TRUE, FALSE)
    ) #%>%  ##this is probably only accurate for scale = "log"
    #print()




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
  model <- survival::survreg(
    formula,  ##Make this chunk into an argument of the function
    weights = `P(C=c|y,t)`,
    data = df_temp,
    dist = "gaussian",
    control = survreg.control(maxiter = maxiter_survreg, debug = verbose > 3))


  if (model$iter == maxiter_survreg){
    likelihood_documentation[i,3] <- TRUE
  } else{
    likelihood_documentation[i,3] <- FALSE
  }
    #browser()




    newmodel = bind_cols(mean = coef(model) %>% t(), sd = model$scale %>% t())

    nn <- paste("c", 1:ncomp, sep = "")

    ##newmodel2 <- tibble(t(data.frame(split(newmodel$mean, rep_len(nn, length.out = length(newmodel$mean)))))) %>%
     # rename(comp_mean = 1) %>%
     # mutate(sd = c(newmodel$sd)) %>%
     # mutate(names = nn) %>%
     # relocate(names, .before = everything()) %>% print()

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
if(plot_visuals == TRUE){

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
      select(-any_of("P(C = c)")) %>%
      left_join(pi, by = "c") %>%
      mutate(
        `E[Y|t,c]` = predict(model, newdata = possible_data),
        `sd[Y|t,c]` = model$scale[c],
       # `Var[Y|t,c]` = `sd[Y|t,c]`^2,

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
      pi = pi,
      coefficients_and_sd = newmodel,
      steps = i
    )
  )

}
