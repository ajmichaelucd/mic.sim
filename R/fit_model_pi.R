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
    int <- cens_counts %>% pull(int)
    rc <- cens_counts %>% pull(rc)
    `P(1)` <- (lc + (0.5 * int) + 1) / (n_obs + 2)
    `P(2)` <- (rc + (0.5 * int) + 1) / (n_obs + 2)

    visible_data %>%
      reframe(.by = everything(),
              c = as.character(1:2)
      ) %>%
      mutate(
        `E[Y|t,c]` = case_when(c == "1" ~ low_con,
                               c == "2" ~ high_con,
                               TRUE ~ NaN),
        `sd[Y|t,c]` = case_when(c == "1" ~ 0.2 * (high_con - low_con),
                                c == "2" ~  0.2 * (high_con - low_con),
                                TRUE ~ NaN),
        `P(Y|t,c)` =  if_else(
          left_bound == right_bound,
          dnorm(x = left_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`),
          pnorm(right_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`) -
            pnorm(left_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`)
        )
      ) %>%
      group_by(obs_id) %>%
      mutate(`P(C=c|t)` = case_when(
        c == "2" ~ `P(2)`, ########UNSURE ABOUT THIS SECTION
        c == "1" ~ `P(1)` ########UNSURE ABOUT THIS SECTION
      )) %>%  ########UNSURE ABOUT THIS SECTION
      mutate(`P(c,y|t)` = `P(C=c|t)` * `P(Y|t,c)`, ########UNSURE ABOUT THIS SECTION
             `P(Y=y|t)` = sum(`P(c,y|t)`), ########UNSURE ABOUT THIS SECTION
             `P(C=c|y,t)` = `P(c,y|t)` / `P(Y=y|t)`) %>%  ########UNSURE ABOUT THIS SECTION
      ungroup() -> possible_data

if(plot_visuals){
    data.plot = possible_data %>% mutate(cens =
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


    #mean <-
      data.plot %>% ggplot() +
      #geom_bar(aes(x = mid, fill = cens)) +
     # geom_point(aes(x = t, y = mid, color = `P(C=c|y,t)`), data = data.plot %>% filter(c == "2"), alpha = 0) +
        geom_ribbon(aes(ymin = low_con - 1.96 * 0.3*(high_con - low_con), ymax = low_con + 1.96 * 0.3*(high_con - low_con), x = t, fill = "Component 1 Mu"), data = ci_data, alpha = 0.25) +
        geom_ribbon(aes(ymin = high_con - 1.96 * 0.3*(high_con - low_con), ymax = high_con + 1.96 * 0.3*(high_con - low_con), x = t, fill = "Component 2 Mu"), data = ci_data, alpha = 0.25) +
        geom_hline( aes(yintercept = high_con, color = "Component 2 Mean")) +
        geom_hline( aes(yintercept = low_con, color = "Component 1 Mean")) +
        ggnewscale::new_scale_color() +
       # scale_color_gradient2(low = "purple", high = "darkorange", mid = "green", midpoint = 0.5) +
        scale_color_gradient2(low = "red", high = "blue", mid = "green", midpoint = 0.5) +
      geom_segment(aes(x = t, xend = t, y = left_bound, yend = right_bound, color = `P(C=c|y,t)`), data = (data.plot %>% filter(cens == "int" & c == "2")), alpha = 0.3) +
      geom_segment(aes(x = t, xend = t, y = right_bound, yend = left_bound, color = `P(C=c|y,t)`), data = (data.plot %>% filter(cens == "lc" & c == "2") %>% mutate(plot_min)), arrow = arrow(length = unit(0.03, "npc")), alpha = 0.3) +
      geom_segment(aes(x = t, xend = t, y = left_bound, yend = right_bound, color = `P(C=c|y,t)`), data = (data.plot %>% filter(cens == "rc" & c == "2") %>% mutate(plot_max)), arrow = arrow(length = unit(0.03, "npc")), alpha = 0.3) +
      geom_point(aes(x = t, y = left_bound,  color = `P(C=c|y,t)`), data = data.plot %>% filter(left_bound != -Inf & c == "2"), alpha = 0.3) +
      geom_point(aes(x = t, y = right_bound,  color = `P(C=c|y,t)`), data = data.plot %>% filter(right_bound != Inf & c == "2"), alpha = 0.3) +
      #scale_colour_gradientn(colours = c("purple", "darkorange")) +
      #ylim(plot_min - 0.5, plot_max + 0.5) +
      ggtitle(paste0("Initial Condition")) +
      xlab("Time") +
      ylab("MIC") +
      ylim(low_con -  2 * 0.5*(high_con - low_con) , high_con + 2* 0.5*(high_con - low_con))

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

    df_temp <- possible_data %>% filter(`P(C=c|y,t)` > 0 )
    #possible_data <- possible_data %>% filter(`P(C=c|y,t)` != 0 )




    #print("about to survreg")

    df_temp %>% filter(c == 1) -> df1
    df_temp %>% filter(c == 2) -> df2
    modelsplit_1 <- survival::survreg(
      formula,  ##Make this chunk into an argument of the function
      weights = `P(C=c|y,t)`,
      data = df1,
      dist = "gaussian",
      control = survreg.control(maxiter = maxiter_survreg, debug = verbose > 3))
    modelsplit_2 <- survival::survreg(
      #formula,  ##Make this chunk into an argument of the function
      formula = formula,
        #Surv(time = left_bound, time2 = right_bound, type = "interval2") ~ pspline(t, df = 0, calc = TRUE, Boundary.knots = c(5.01, 16)),
      weights = `P(C=c|y,t)`,
      data = df2,
      dist = "gaussian",
      control = survreg.control(maxiter = maxiter_survreg, debug = verbose > 3))
    #split model apart because p-spline and strata didn't work together I believe?


    #model <- survival::survreg(
    #  formula,  ##Make this chunk into an argument of the function
    #  weights = `P(C=c|y,t)`,
    #  data = df_temp,
    #  dist = "gaussian",
    #  control = survreg.control(maxiter = maxiter_survreg, debug = verbose > 3))
#summary(modelsplit_1)
#summary(modelsplit_2)

    if (modelsplit_1$iter[1] == maxiter_survreg | modelsplit_2$iter[1] == maxiter_survreg){
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



    newmodel = list(modelsplit_1, modelsplit_2)

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

    if(pi_link == "logit"){
      binom_model <- gam::gam(formula2, family = binomial(link = "logit"), data = possible_data, weights = `P(C=c|y,t)`)
    } else if(pi_link == "identity"){

      binom_model <- gam::gam(formula2, family = binomial(link = "identity"), data = possible_data, weights = `P(C=c|y,t)`)
    } else{ errorCondition("pick logit or identity link function")}


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

if(is.na(mu_coef_diff) | (tibble(a = modelsplit_1$coefficients) %>% filter(is.na(a)) %>% nrow + tibble(a = modelsplit_2$coefficients) %>% filter(is.na(a)) %>% nrow) > 0){
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
 #   if(plot_visuals == TRUE){
###outdated
 #     c1_plot <-   possible_data %>%
 #       filter(c == 1) %>%
 #       ggplot(mapping = aes(x = t, y = mid, color = `P(C=c|y,t)`)) +
 #       geom_point() +
 #       geom_abline(data = NULL, intercept = newmodel$mean[,"c1"], slope = newmodel$mean[,"c1:t"], color = "red") +
 #       geom_abline(data = NULL, intercept = newmodel$mean[,"c2"], slope = newmodel$mean[,"c2:t"], color = "violet")+
 #       expand_limits(y = c(newmodel$mean[,"c1"], newmodel$mean[,"c2"]))
 #     print(c1_plot)
 #   }

    #Next E step-------------
    possible_data %<>%
      #     select(-any_of("P(C = c)")) %>%
      #      left_join(pi, by = "c") %>%
      mutate(
        `E[Y|t,c]` = case_when(c == "1" ~ predict(modelsplit_1, newdata = possible_data),
                               c == "2" ~ predict(modelsplit_2, newdata = possible_data),
                               TRUE ~ NaN),
          #predict(model, newdata = possible_data),
        `sd[Y|t,c]` = case_when(c == "1" ~ modelsplit_1$scale,
                                c == "2" ~ modelsplit_2$scale, #1,
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
      group_by(obs_id) %>% #remove, use .by or by in the mutate step (combine the two mutates)
      #  mutate(
      #    `P(c,y|t)` = `P(Y|t,c)` * `P(C = c)`, #unsure here
      #    `P(Y=y|t)` = sum(`P(c,y|t)`),
      #    `P(C=c|y,t)` = `P(c,y|t)` / `P(Y=y|t)`
      #  )
      mutate( `P(C=c|t)` = case_when( ########UNSURE ABOUT THIS SECTION
        c == "2" ~ predict(binom_model, newdata = tibble(t = t), type = "response"), ########UNSURE ABOUT THIS SECTION
        c == "1" ~ 1 - predict(binom_model, newdata = tibble(t = t), type = "response") ########UNSURE ABOUT THIS SECTION
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
      summarise(.by = obs_id, likelihood_i = sum(`P(c,y|t)`)) %>%
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

if(i > 1 && likelihood_documentation[i, 2] < likelihood_documentation[i - 1, 2]){
  browser("likelihood decreased")
}




    if(browse_each_step & plot_visuals){

      #plot_fm_step(binom_model, newmodel, ncomp, possible_data, prior_step_plot, i)







      browser(message("End of step ", i))
      plot_fm_step(binom_model, newmodel, ncomp, possible_data, prior_step_plot, i)

      if(i > 1){
      plot_likelihood(likelihood_documentation, format = "matrix")
}
    }
    if(browse_each_step & !plot_visuals){browser(message("End of step ", i))}
    if(i != 1)
    {

      check_ll = log_likelihood - old_log_likelihood

      check_ll_2 = check_ll < tol_ll

      #    other_plot <- tibble(likelihood_documentation) %>% ggplot() +
      #      geom_line(aes(x = likelihood_documentation[,1],
      #                    y = likelihood_documentation[,2]))
      #
      #    print(grid.arrange(c1_plot, other_plot, nrow = 1))

      if(check_ll < 0 ){
        warning("Log Likelihood decreased")  ###  HAS BEEN GETTING USED A LOT, WHY IS THE LOG LIKELIHOOD GOING DOWN????
      }



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

if(i == max_it & !(check_ll_2 & param_checks)){
  converge = "iterations"
}

  return(
    list(
      #likelihood = likelihood_documentation[1:i, ] %>% as_tibble %>% suppressWarnings() %>% rename(step = V1, likelihood = V2, survreg_maxout = V3) %>% filter(!is.na(likelihood)),
      #likelihood = likelihood_documentation[1:i, ],
      likelihood = tibble_like(likelihood_documentation),
      possible_data = possible_data,
      binom_model = binom_model,
      newmodel = newmodel,
      steps = i,
      converge = converge,
      ncomp = ncomp
    )
  )
  }
}

tibble_like <- function(likelihood_documentation){
  likelihood_documentation %>% as_tibble %>% suppressWarnings() %>% rename(step = V1, likelihood = V2, survreg_maxout = V3) %>% filter(!is.na(likelihood)) %>% return()
}
