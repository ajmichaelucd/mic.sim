capture_error_measures_one_run_both_directions <- function(individual_run,
                                                           intercepts,
                                                           trends,
                                                           sigma,
                                                           pi,
                                                           sigma_tolerance = c(.05, 100),
                                                           pi_tolerance = c(.05, .95),
                                                           intercepts_tolerance = 100,
                                                           trends_tolerance = 100 ){

  if(tail(intercepts, 1) < head(intercepts, 1)){ errorCondition("Incorrect order of intercepts parameter, please start with lower one first")}

  if(length(individual_run) == 2 && individual_run[[1]] == "Error"){return(tibble(comp = "Error", parameter = "Error", est = "Error", true = "Error", error = "Error", iter = individual_run[[3]], steps = NaN,sigma_error = "TRUE", pi_error = "TRUE", intercept_error = "TRUE", trends_error = "TRUE", survreg_failure_last = TRUE, survreg_failure_any = TRUE, num_it = NA_integer_))
  } else{


    #  a <- min_rank(abs(intercepts - individual_run[[4]]$mean[1]))
    #  b <- min_rank(abs(intercepts - individual_run[[4]]$mean[2])) #this just picks which is closer to truth, might want to change to just run both orders and pick afterwards

    sr_any <- as_tibble(individual_run[[1]]$likelihood, .name_repair = "unique") %>% pull(`...3`) %>% as.logical() %>% any()
    sr_last <- as_tibble(individual_run[[1]]$likelihood, .name_repair = "unique") %>% mutate(sr = as.logical(`...3`)) %>% select(sr) %>% slice_tail(n = 1) %>% pull


    a <- c(1,2)
    b <- c(2,1)

    forward  <- tibble(
      comp = c("c1", "c2"),
      est_intercepts = individual_run[[4]]$mean[a],
      true_intercepts = intercepts,
      est_trends = individual_run[[4]]$mean[a + 2],
      true_trends = trends,
      est_sigma = c(individual_run[[4]]$sd),
      true_sigma = sigma,
      est_pi = individual_run[[3]]$`P(C = c)`,
      true_pi = pi) %>%
      mutate(
        error_intercepts = true_intercepts - est_intercepts,
        error_trends = true_trends - est_trends,
        error_sigma = true_sigma - est_sigma,
        error_pi = true_pi - est_pi
      ) %>%
      tidyr::pivot_longer(cols = est_intercepts:error_pi) %>%
      separate(name, sep = "_", into = c("type", "parameter")) %>%
      pivot_wider(names_from = type, values_from = value) %>%
      mutate(iter = individual_run[[5]],
             steps = nrow(individual_run[[1]]))


    reverse  <- tibble(
      comp = c("c1", "c2"),
      est_intercepts = individual_run[[4]]$mean[b],
      true_intercepts = intercepts,
      est_trends = individual_run[[4]]$mean[b + 2],
      true_trends = trends,
      est_sigma = rev(individual_run[[4]]$sd),
      true_sigma = sigma,
      est_pi = rev(individual_run[[3]]$`P(C = c)`),
      true_pi = pi) %>%
      mutate(
        error_intercepts = true_intercepts - est_intercepts,
        error_trends = true_trends - est_trends,
        error_sigma = true_sigma - est_sigma,
        error_pi = true_pi - est_pi
      ) %>%
      tidyr::pivot_longer(cols = est_intercepts:error_pi) %>%
      separate(name, sep = "_", into = c("type", "parameter")) %>%
      pivot_wider(names_from = type, values_from = value) %>%
      mutate(iter = individual_run[[5]],
             steps = nrow(individual_run[[1]]))

    f_error <- forward %>% summarize(total_error = sum(abs(error)))
    f_error <- f_error$total_error
    r_error <- reverse %>% summarize(total_error = sum(abs(error)))
    r_error <- r_error$total_error

    # if(min(c(f_error, r_error)) > error_threshold){return(tibble(comp = "Error", parameter = "Error", est = "Error", true = "Error", error = "Error", iter = "Error"))}
    #
    #     if(f_error < r_error){return(forward)}
    #     else if(f_error > r_error){return(reverse)}
    #     else{warningCondition("We have ourselves an issue in determining which version has lower error")
    #       return(tibble(comp = "Error", parameter = "Error", est = "Error", true = "Error", error = "Error", iter = "Error"))}


    if(f_error < r_error){df <- forward
    selection = "f"} else if(f_error > r_error){df <- reverse
    selection = "r"} else{warningCondition("We have ourselves an issue in determining which version has lower error")
      df <- forward
      #selection = "e"
    }

  }

  a <- df %>% filter(parameter == "sigma")
  if(max(abs(a$est)) > max(sigma_tolerance) | min(abs(a$est)) < min(sigma_tolerance) ){sigma_error = TRUE
  } else{sigma_error = FALSE}

  a <- df %>% filter(parameter == "pi")
  if(max(a$est) >= max(pi_tolerance) | min(a$est) <= min(pi_tolerance)){pi_error = TRUE
  } else{pi_error = FALSE}

  a <- df %>% filter(parameter == "intercepts")
  if(max(abs(a$est)) >= intercepts_tolerance){intercept_error = TRUE
  } else{intercept_error = FALSE}

  a <- df %>% filter(parameter == "trends")
  if(max(abs(a$est)) >= trends_tolerance){trends_error = TRUE
  } else{trends_error = FALSE}

  df2 <-
    df %>% mutate(
      sigma_error = sigma_error,
      pi_error = pi_error,
      intercept_error = intercept_error,
      trends_error = trends_error,
      survreg_failure_last = sr_last,
      survreg_failure_any = sr_any,
      num_it = individual_run[[1]]$steps
    )

  return(df2)

}
