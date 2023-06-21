simulate_mics_X <- function(n = 100,
                          t_dist = function(n){runif(n, min = 0, max = 1)},
                          pi = function(t) {z <- 0.5
                          c("1" = z, "2" = 1- z)},
                          `E[X|T,C]` = function(t, c)
                          {
                            case_when(
                              c == "1" ~ 3 + t + 2*t^2 - sqrt(t),
                              c == "2" ~ 3*t,
                              TRUE ~ NaN
                            )
                          },
                          sd_vector = c("1" = 1, "2" = 2),
                          covariate_list = list(c("numeric", "normal", 0, 1), c("categorical", c(0.3, 0.4)), c("numeric", "uniform", 0, 5)), #NULL
                          covariate_effect_vector = c(0, 1, 0.2, 0.1), #c(0)
                          #concentrations = tibble() #covariates, high and low con
                          conc_tibble = tibble(covariate_2 = c("a", "b"), low_con = c(2^-4, 2^-3), high_con = c(2^4, 2^4)), #NULL,
                          tested_concentrations = NULL, #log2(low_con):log2(high_con),
                          scale = "log"){
  if(is.null(covariate_list)){
    base_data <- draw_epsilon(n, t_dist, pi, `E[X|T,C]`, sd_vector)
    simulated_obs <- base_data %>% mutate(observed_value = epsilon + x)
    censored_obs <- censor_values_2(simulated_obs$observed_value, conc_tibble, tested_concentrations, scale)
    df <- inner_join(simulated_obs, censored_obs, by = "observed_value")
    attr(df, "scale") <- scale
    return(df)
  } else{
    base_data <- draw_epsilon(n, t_dist, pi, `E[X|T,C]`, sd_vector)
    covariate_data <- add_covariate(covariate_list = covariate_list, input = base_data$t)
    merged_data <- tibble(base_data, covariate_data)
    total_cov_effect <- covariate_effect_total(merged_data, covariate_effect_vector)
    simulated_obs <- tibble(merged_data, total_cov_effect) %>%
      mutate(observed_value = epsilon + total_cov_effect + x)
    censored_obs <- censor_values_2(simulated_obs$observed_value, conc_tibble, tested_concentrations, scale)
    df <- tibble(simulated_obs, censored_obs)
    attr(df, "scale") <- scale

    #####X COLUMNS OF SIMULATED OBS AND CENSORED OBS ARE DIFFERENT, CENSORED OBS SEEMS TO HAVE X AS 0 OR 5.5 FOR SOME REASON??

    return(censored_obs)
  }
}


censor_values_2 <-
  function(
    observed_value,
    conc_tibble, #if conc_tibble is null, then just use low_con, high_con
    low_con = 2^-4,
    high_con = 2^4,
    tested_concentrations = log2(low_con):log2(high_con),
    scale = "log"
  ){
    if(is.null(conc_tibble)){
    if(scale == "MIC"){
      df = dplyr::tibble(
        left_bound = sapply(observed_value, function(x) max(tested_concentrations[tested_concentrations < x])) %>% suppressWarnings(),
        observed_value,
        right_bound = sapply(observed_value, function(x) min(tested_concentrations[tested_concentrations >= x])) %>%
          suppressWarnings(),
        indicator = dplyr::case_when(
          is.finite(right_bound) & is.finite(left_bound) ~ 3,
          is.finite(right_bound) & is.infinite(left_bound) ~ 2,
          is.infinite(right_bound) & is.finite(left_bound) ~ 0
        )
      ) %>%
        mutate(
          left_bound = 2^left_bound,
          right_bound = 2^right_bound
        )

      return(df)
    }

    else if(scale == "log"){
      df = dplyr::tibble(
        left_bound = sapply(observed_value, function(x) max(tested_concentrations[tested_concentrations < x])) %>% suppressWarnings(),
        observed_value,
        right_bound = sapply(observed_value, function(x) min(tested_concentrations[tested_concentrations >= x])) %>%
          suppressWarnings(),
        indicator = dplyr::case_when(
          is.finite(right_bound) & is.finite(left_bound) ~ 3,
          is.finite(right_bound) & is.infinite(left_bound) ~ 2,
          is.infinite(right_bound) & is.finite(left_bound) ~ 0
        )
      )

      return(df)
    }
    else{warningCondition(message = "Choose scale: MIC or log")}
    } else{
      if(scale == "MIC"){
        r <- conc_tibble %>% rowwise %>% mutate(tested_concentrations = list(log2(low_con):log2(high_con)))  # %>% select(tested_concentrations)
        f <- left_join(simulated_obs, r) #%>% rowwise() %>% mutate(left_bound = max(tested_concentrations[tested_concentrations < observed_value]))
        b <- matrix(nrow = nrow(f), ncol = 2)
        for(i in 1:nrow(f)){
          ob <- c(f[i,]$observed_value)
          tc <- f[i,]$tested_concentrations[[1]]
          lb <- max(tc[tc < ob]) %>% suppressWarnings()
          ub <- min(tc[tc >= ob]) %>% suppressWarnings()
          b[i, 1] <- lb
          b[i, 2] <- ub


        }
        bounds <- tibble(left_bound = b[,1], right_bound = b[,2])
        df = tibble(f, bounds) %>% mutate(
        #df = dplyr::tibble(
        #  left_bound = sapply(observed_value, function(x) max(tested_concentrations[tested_concentrations < x])) %>% suppressWarnings(),
        #  observed_value,
        #  right_bound = sapply(observed_value, function(x) min(tested_concentrations[tested_concentrations >= x])) %>%
        #    suppressWarnings(),
          indicator = dplyr::case_when(
            is.finite(right_bound) & is.finite(left_bound) ~ 3,
            is.finite(right_bound) & is.infinite(left_bound) ~ 2,
            is.infinite(right_bound) & is.finite(left_bound) ~ 0
          )
        ) %>%
          mutate(
            left_bound = 2^left_bound,
            right_bound = 2^right_bound
          )





        return(df)
      }

      else if(scale == "log"){

        r <- conc_tibble %>% rowwise %>% mutate(tested_concentrations = list(log2(low_con):log2(high_con)))  # %>% select(tested_concentrations)
        f <- left_join(simulated_obs, r) #%>% rowwise() %>% mutate(left_bound = max(tested_concentrations[tested_concentrations < observed_value]))
        b <- matrix(nrow = nrow(f), ncol = 2)
        for(i in 1:nrow(f)){
          ob <- c(f[i,]$observed_value)
          tc <- f[i,]$tested_concentrations[[1]]
          lb <- max(tc[tc < ob]) %>% suppressWarnings()
          ub <- min(tc[tc >= ob]) %>% suppressWarnings()
          b[i, 1] <- lb
          b[i, 2] <- ub


        }
        bounds <- tibble(left_bound = b[,1], right_bound = b[,2])
        df = tibble(f, bounds) %>% mutate(
          #df = dplyr::tibble(
          #  left_bound = sapply(observed_value, function(x) max(tested_concentrations[tested_concentrations < x])) %>% suppressWarnings(),
          #  observed_value,
          #  right_bound = sapply(observed_value, function(x) min(tested_concentrations[tested_concentrations >= x])) %>%
          #    suppressWarnings(),
          indicator = dplyr::case_when(
            is.finite(right_bound) & is.finite(left_bound) ~ 3,
            is.finite(right_bound) & is.infinite(left_bound) ~ 2,
            is.infinite(right_bound) & is.finite(left_bound) ~ 0
          )
        )

        return(df)
      }
  }

  }
