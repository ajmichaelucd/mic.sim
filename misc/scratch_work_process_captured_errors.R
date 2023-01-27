load("~/Desktop/censor_mean_2_sd_0.8_pi2_0.8_run_16_01032023_1.Rdata")

results[[1]] #survreg failure
results[[3]] #error
results[[7]] #successful convergence (albeit wrong but still)

location <- "~/Desktop/"
format <- "name_date_number"
array_name <- "censor_mean_2_sd_0.8_pi2_0.8_run_16"
date <- "01032023"
i = 1 #map iterates over this

batch_size <- 10
parameter_log <- read_excel("~/Desktop/Sim_Results/simulation_parameter_log.xlsx",
                            sheet = "Sheet2")

#thresholds
sigma_tolerance = c(0.25, 50)
pi_tolerance = c(0.05, 0.95)
intercepts_tolerance = 100
trends_tolerance = 100

##extra parameters needed to recreate data sets
covariate_list <-  NULL
covariate_effect_vector <- c(0)
scale = "log"


###Grab target values with function
params <- grab_sim_parameters(parameter_log = parameter_log, array_name = array_name, date = date, id_col = "FULL_NAME")
print(params)
#target values
intercepts = c(params$`C1 Mean`, params$`C2 Mean`)  #c(-1, 2.3)
trends = c(params$`C1 Trend`, params$`C2 Trend`)  #c(0.1, 0)
sigma = c(params$`C1 SD`, params$`C2 SD`)  #c(1, 0.5)
pi = c(params$`C1 Pi`, params$`C2 Pi`)  #c(0.5, 0.5)
low_con = 2^params$min
high_con = 2^params$max

nyears = params$years
n = params$n

capture_error_measures_one_batch <- function(location,
                                     format,
                                     array_name,
                                     date,
                                     i,
                                     batch_size,
                                     intercepts,
                                     trends,
                                     sigma,
                                     pi,
                                     sigma_tolerance = c(.05, 100),
                                     pi_tolerance = c(.05, .95),
                                     intercepts_tolerance = 100,
                                     trends_tolerance = 100
){
  file  <- gen_path_sim(location = location, format = format, array_name = array_name, date = date, i = i)
  results_pre <- loadRData(file)

  results <- purrr::map2(1:batch_size, results_pre, ~append(.y, (batch_size*(i - 1) + .x )))

  #note results[[3]] failure is not list object

  purrr::map(results, ~error_measures_one_run_both_directions(.x, intercepts, trends, sigma, pi, sigma_tolerance, pi_tolerance, intercepts_tolerance, trends_tolerance)) %>%
    data.table::rbindlist()
      ##add attribute read to the error_measures_one_run_both_directions segment

}

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

  if(length(individual_run) == 2 && individual_run[[1]] == "Error"){return(tibble(comp = "Error", parameter = "Error", est = "Error", true = "Error", error = "Error", iter = individual_run[[2]], steps = NaN,sigma_error = "TRUE", pi_error = "TRUE", intercept_error = "TRUE", trends_error = "TRUE"))
  } else{


    #  a <- min_rank(abs(intercepts - individual_run[[4]]$mean[1]))
    #  b <- min_rank(abs(intercepts - individual_run[[4]]$mean[2])) #this just picks which is closer to truth, might want to change to just run both orders and pick afterwards

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
    selection = "f"}
    else if(f_error > r_error){df <- reverse
    selection = "r"}
    else{warningCondition("We have ourselves an issue in determining which version has lower error")
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

  df2 <- df %>% mutate(sigma_error = sigma_error, pi_error = pi_error, intercept_error = intercept_error, trends_error = trends_error)

  return(df2)

}
