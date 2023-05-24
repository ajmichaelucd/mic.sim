#' Title
#'
#' @param individual_run
#' @param intercepts
#' @param trends
#' @param sigma
#' @param pi
#' @param sigma_tolerance
#' @param pi_tolerance
#' @param intercepts_tolerance
#' @param trends_tolerance
#'
#' @return
#' @export
#'
#' @examples
capture_error_measures_one_run <- function(individual_run,
                                                                  intercepts,
                                                                  trends,
                                                                  sigma,
                                                                  pi_int,
                                                                  pi_trend,
                                                                  sigma_tolerance = c(.05, 100),
                                                                  pi_int_tolerance = c(0.0001, .9999),
                                                                  pi_trend_tolerance = c(-4, 4),
                                                                  intercepts_tolerance = 100,
                                                                  trends_tolerance = 100 ){

  ###WILL BE ISSUES WITH LOW VS HIGH COMPONENT WHEN ONLY ONE COMPONENT ESTIMATED####A
  x <- length(individual_run)



  if((individual_run[[x-1]] == "fm_worked" & individual_run[[x-3]] == FALSE) | (individual_run[[x-1]] == "fm_failed_cutoff" & individual_run[[x]] == "fms_not_called")){

    if(tail(intercepts, 1) < head(intercepts, 1)){ errorCondition("Incorrect order of intercepts parameter, please start with lower one first")}

    # if(length(individual_run) == 2 && individual_run[[1]] == "Error"){return(tibble(comp = "Error", parameter = "Error", est = "Error", true = "Error", error = "Error", iter = individual_run[[2]], steps = NA_integer_,sigma_error = "TRUE", pi_error = "TRUE", intercept_error = "TRUE", trends_error = "TRUE", survreg_failure_last = TRUE, survreg_failure_any = TRUE, num_it = NA_integer_))
    # } else{


    #  a <- min_rank(abs(intercepts - individual_run[[4]]$mean[1]))
    #  b <- min_rank(abs(intercepts - individual_run[[4]]$mean[2])) #this just picks which is closer to truth, might want to change to just run both orders and pick afterwards

    sr_any <- as_tibble(individual_run$likelihood, .name_repair = "unique") %>% pull(`...3`) %>% as.logical() %>% any()
    sr_last <- as_tibble(individual_run$likelihood, .name_repair = "unique") %>% mutate(sr = as.logical(`...3`)) %>% select(sr) %>% slice_tail(n = 1) %>% pull


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
      est_pi_int = individual_run[[3]]$`(Intercept)`,
      true_pi_int = pi_int,
      est_pi_trend = individual_run[[3]]$t,
      true_pi_trend = pi_trend
    ) %>%
      mutate(
        error_intercepts = true_intercepts - est_intercepts,
        error_trends = true_trends - est_trends,
        error_sigma = true_sigma - est_sigma,
        error_pi_int = true_pi_int - est_pi_int,
        error_pi_trend = true_pi_trend - est_pi_trend
      ) %>%
      tidyr::pivot_longer(cols = est_intercepts:error_pi_trend) %>%
      separate(name, sep = "_", into = c("type", "parameter")) %>%
      pivot_wider(names_from = type, values_from = value) %>%
      mutate(iter = individual_run[[x - 4]],
             steps = nrow(individual_run[[1]]))


    reverse  <- tibble(
      comp = c("c1", "c2"),
      est_intercepts = individual_run[[4]]$mean[b],
      true_intercepts = intercepts,
      est_trends = individual_run[[4]]$mean[b + 2],
      true_trends = trends,
      est_sigma = rev(individual_run[[4]]$sd),
      true_sigma = sigma,
      est_pi_int = individual_run[[3]]$`(Intercept)`,
      true_pi_int = pi_int,
      est_pi_trend = individual_run[[3]]$t,
      true_pi_trend = pi_trend) %>%
      mutate(
        error_intercepts = true_intercepts - est_intercepts,
        error_trends = true_trends - est_trends,
        error_sigma = true_sigma - est_sigma,
        error_pi_int = true_pi_int - est_pi_int,
        error_pi_trend = true_pi_trend - est_pi_trend
      ) %>%
      tidyr::pivot_longer(cols = est_intercepts:error_pi_trend) %>%
      separate(name, sep = "_", into = c("type", "parameter")) %>%
      pivot_wider(names_from = type, values_from = value) %>%
      mutate(iter = individual_run[[x - 4]],
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


    #}

    a <- df %>% filter(parameter == "sigma")
    if(max(abs(a$est)) > max(sigma_tolerance) | min(abs(a$est)) < min(sigma_tolerance) ){sigma_error = TRUE
    } else{sigma_error = FALSE}

    a <- df %>% filter(parameter == "pi_int")
    if(max(a$est) >= max(pi_int_tolerance) | min(a$est) <= min(pi_int_tolerance)){pi_int_error = TRUE
    } else{pi_int_error = FALSE}

    a <- df %>% filter(parameter == "pi_trend")
    if(max(a$est) >= max(pi_trend_tolerance) | min(a$est) <= min(pi_trend_tolerance)){pi_trend_error = TRUE
    } else{pi_trend_error = FALSE}

    a <- df %>% filter(parameter == "intercepts")
    if(max(abs(a$est)) >= intercepts_tolerance){intercept_error = TRUE
    } else{intercept_error = FALSE}

    a <- df %>% filter(parameter == "trends")
    if(max(abs(a$est)) >= trends_tolerance){trends_error = TRUE
    } else{trends_error = FALSE}

    df2 <-
      df %>% mutate(
        sigma_error = sigma_error,
        pi_int_error = pi_int_error,
        pi_trend_error = pi_trend_error,
        intercept_error = intercept_error,
        trends_error = trends_error,
        survreg_failure_last = sr_last,
        survreg_failure_any = sr_any,
        num_it = individual_run$steps,
        fms_only = individual_run[[x-3]],
        safety_on = individual_run[[x - 2]],
        fm = individual_run[[x - 1]],
        fms_called = case_when(
          individual_run[[x]] %in% c("fms_worked", "fms_failed") ~ TRUE,
          TRUE ~ FALSE
        ),
        fms_worked = case_when(
          individual_run[[x]] == "fms_worked" ~ TRUE,
          TRUE ~ FALSE
        )
      )

    return(df2)


  }  else if(individual_run[[x-1]] == "fm_worked" & individual_run[[x-3]] == TRUE){
    return(
      tibble(
        comp = "Pass",
        parameter = "Pass",
        est = "Pass",
        true = "Pass",
        error = "Pass",
        iter = individual_run[[x - 4]],
        steps = NA_integer_,
        sigma_error = "Pass",
        pi_int_error = "FALSE",
        pi_trend_error = "FALSE",
        intercept_error = "FALSE",
        trends_error = "FALSE",
        survreg_failure_last = FALSE,
        survreg_failure_any = FALSE,
        num_it = NA_integer_,
        fms_only = individual_run[[x-3]],
        safety_on = individual_run[[x - 2]],
        fm = individual_run[[x - 1]],
        fms_called = case_when(
          individual_run[[x]] %in% c("fms_worked", "fms_failed") ~ TRUE,
          TRUE ~ FALSE
        ),
        fms_worked = case_when(
          individual_run[[x]] == "fms_worked" ~ TRUE,
          TRUE ~ FALSE
        )
      )
    )
  }else if(individual_run[[x-1]] == "fm_failed" & individual_run[[x]] == "fms_not_called"){


    return(
      tibble(
        comp = "Error",
        parameter = "Error",
        est = "Error",
        true = "Error",
        error = "Error",
        iter = individual_run[[x - 4]],
        steps = NA_integer_,
        sigma_error = "TRUE",
        pi_int_error = "FALSE",
        pi_trend_error = "FALSE",
        intercept_error = "TRUE",
        trends_error = "TRUE",
        survreg_failure_last = TRUE,
        survreg_failure_any = TRUE,
        num_it = NA_integer_,
        fms_only = individual_run[[x-3]],
        safety_on = individual_run[[x - 2]],
        fm = individual_run[[x - 1]],
        fms_called = case_when(
          individual_run[[x]] %in% c("fms_worked", "fms_failed") ~ TRUE,
          TRUE ~ FALSE
        ),
        fms_worked = case_when(
          individual_run[[x]] == "fms_worked" ~ TRUE,
          TRUE ~ FALSE
        )
      )
    )

  } else if(individual_run[[x-1]] %in% c("fm_failed", "fm_failed_cutoff") & individual_run[[x]] == "fms_worked"){

    sr_any <- as_tibble(individual_run$likelihood, .name_repair = "unique") %>% pull(`...3`) %>% as.logical() %>% any()
    sr_last <- as_tibble(individual_run$likelihood, .name_repair = "unique") %>% mutate(sr = as.logical(`...3`)) %>% select(sr) %>% slice_tail(n = 1) %>% pull

    ##add an if_else here to choose c1 or c2 and use that to select intercepts, trends, sigma, pi, etc

    df <- tibble(
      comp = c("c1"),
      est_intercepts = individual_run[[4]]$mean[1],
      true_intercepts = intercepts [1],
      est_trends = individual_run[[4]]$mean[2],
      true_trends = trends[1],
      est_sigma = individual_run[[4]]$sd,
      true_sigma = sigma[1],
      est_pi_int = individual_run[[3]]$`(Intercept)`,
      true_pi_int = pi_int,
      est_pi_trend = individual_run[[3]]$t,
      true_pi_trend = pi_trend) %>%
      mutate(
        error_intercepts = true_intercepts - est_intercepts,
        error_trends = true_trends - est_trends,
        error_sigma = true_sigma - est_sigma,
        error_pi_int = true_pi_int - est_pi_int,
        error_pi_trend = true_pi_trend - est_pi_trend
      ) %>%
      tidyr::pivot_longer(cols = est_intercepts:error_pi) %>%
      separate(name, sep = "_", into = c("type", "parameter")) %>%
      pivot_wider(names_from = type, values_from = value) %>%
      mutate(iter = individual_run[[x - 4]],
             steps = nrow(individual_run[[1]]))



    a <- df %>% filter(parameter == "sigma")
    if((abs(a$est) > max(sigma_tolerance)) | (abs(a$est) < min(sigma_tolerance)) ){sigma_error = TRUE
    } else{sigma_error = FALSE}

    a <- df %>% filter(parameter == "pi_int")
    if(a$est >= max(pi_int_tolerance) | a$est <= min(pi_int_tolerance)){pi_int_error = TRUE
    } else{pi_int_error = FALSE}

    a <- df %>% filter(parameter == "pi_trend")
    if(a$est >= max(pi_trend_tolerance) | a$est <= min(pi_trend_tolerance)){pi_trend_error = TRUE
    } else{pi_trend_error = FALSE}

    a <- df %>% filter(parameter == "intercepts")
    if(abs(a$est) >= intercepts_tolerance){intercept_error = TRUE
    } else{intercept_error = FALSE}

    a <- df %>% filter(parameter == "trends")
    if(abs(a$est) >= trends_tolerance){trends_error = TRUE
    } else{trends_error = FALSE}




    df2 <-
      df %>% mutate(
        sigma_error = sigma_error,
        pi_int_error = pi_int_error,
        pi_trend_error = pi_trend_error,
        intercept_error = intercept_error,
        trends_error = trends_error,
        survreg_failure_last = sr_last,
        survreg_failure_any = sr_any,
        num_it = individual_run$steps,
        fms_only = individual_run[[x-3]],
        safety_on = individual_run[[x - 2]],
        fm = individual_run[[x - 1]],
        fms_called = case_when(
          individual_run[[x]] %in% c("fms_worked", "fms_failed") ~ TRUE,
          TRUE ~ FALSE
        ),
        fms_worked = case_when(
          individual_run[[x]] == "fms_worked" ~ TRUE,
          TRUE ~ FALSE
        )
      )

    return(df2)

  } else if(individual_run[[x-1]]  %in% c("fm_failed", "fm_failed_cutoff") & individual_run[[x]] == "fms_failed"){


    return(
      tibble(
        comp = "Error",
        parameter = "Error",
        est = "Error",
        true = "Error",
        error = "Error",
        iter = individual_run[[x - 4]],
        steps = NA_integer_,
        sigma_error = "TRUE",
        pi_int_error = "TRUE",
        pi_trend_error = "TRUE",
        intercept_error = "TRUE",
        trends_error = "TRUE",
        survreg_failure_last = TRUE,
        survreg_failure_any = TRUE,
        num_it = NA_integer_,
        fms_only = individual_run[[x-3]],
        safety_on = individual_run[[x - 2]],
        fm = individual_run[[x - 1]],
        fms_called = case_when(
          individual_run[[x]] %in% c("fms_worked", "fms_failed") ~ TRUE,
          TRUE ~ FALSE
        ),
        fms_worked = case_when(
          individual_run[[x]] == "fms_worked" ~ TRUE,
          TRUE ~ FALSE
        )
      )
    )

  }













}
