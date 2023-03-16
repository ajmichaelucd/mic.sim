#' Title
#'
#' @param n
#' @param t_dist
#' @param pi
#' @param `E[X|T,C]`
#' @param sd_vector
#' @param covariate_list
#' @param covariate_effect_vector
#' @param conc_limits_table
#' @param low_con
#' @param high_con
#' @param scale
#'
#' @return
#' @export
#'
#' @examples
simulate_mics_2 <- function(n = 100,
                            t_dist = function(n){runif(n, min = 0, max = 1)},
                            pi = function(t) {z <- 0.5 + 0.2 * t
                            c("1" = z, "2" = 1- z)},
                            `E[X|T,C]` = function(t, c)
                            {
                              case_when(
                                c == "1" ~ -1 + t,
                                c == "2" ~ 1 + 0.3*t,
                                TRUE ~ NaN
                              )
                            },
                            sd_vector = c("1" = 1, "2" = 0.7),
                            covariate_list = list(c("numeric", "normal", 0, 1), c("categorical", c(0.3, 0.4))),
                            covariate_effect_vector = c(0, #intercept for all covariates combined
                                                        0.2, #slope for covariate_1
                                                        -1), #effect of level b vs a of covariate 2,
                            conc_limits_table = as_tibble(rbind(c("a", 2^-3, 2^3),
                                                                c("b", 2^-4, 2^4)), `.name_repair` = "unique"
                            ) %>% rename("covariate_2" = 1, "low_cons" = 2, "high_cons" = 3),
                            low_con = -4,
                            high_con = 4,
                            #tested_concentrations = log2(low_con):log2(high_con),
                            scale = "log"){
  if(is.null(covariate_list)){
    base_data <- draw_epsilon(n, t_dist, pi, `E[X|T,C]`, sd_vector)
    simulated_obs <- base_data %>% mutate(observed_value = epsilon + x)
    simulated_obs <- simulated_obs %>% mutate(low_con = low_con, high_con = high_con)
    censored_obs <- censor_values(simulated_obs, #simulated_obs$observed_value, #low_con, high_con,
                                  #tested_concentrations,
                                  scale)
    df <- inner_join(simulated_obs, censored_obs) %>% mutate(low_con = as.numeric(low_con), high_con = as.numeric(high_con))
    attr(df, "scale") <- scale
    return(df)
  } else{
    base_data <- draw_epsilon(n, t_dist, pi, `E[X|T,C]`, sd_vector)
    covariate_data <- add_covariate(covariate_list = covariate_list, input = base_data$t)
    merged_data <- tibble(base_data, covariate_data)
    total_cov_effect <- covariate_effect_total(merged_data, covariate_effect_vector)
    simulated_obs <- tibble(merged_data, total_cov_effect) %>%
      mutate(observed_value = epsilon + total_cov_effect + x)
    if(!is.null(conc_limits_table)){
      simulated_obs <- left_join(simulated_obs, conc_limits_table)
    } else{
      simulated_obs <- simulated_obs %>% mutate(low_cons = low_con, high_cons = high_con)
    }

    censored_obs <- censor_values(simulated_obs,
                                  #simulated_obs$observed_value, #low_con, high_con,
                                  #tested_concentrations,
                                  scale)
    df <- inner_join(simulated_obs, censored_obs) %>% mutate(low_con = as.numeric(low_cons), high_con = as.numeric(high_cons)) %>%
      select(-c(low_cons, high_cons))
    attr(df, "scale") <- scale
    return(df)
  }
}

###LATER NEED TO ADD AN OPTION TO CHANGE CONCENTRATIONS BY TIME PERIOD
