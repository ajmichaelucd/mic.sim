#libraries
setwd("~/Desktop/Dissertation Project/Chapter 1/mic.sim")
load_all()
library(dplyr)
library(tidyr)
library(purrr)
library(data.table)
library(fs)
library(ggplot2)
library(stats)
library(magrittr)
library(readxl)
library(LearnBayes)
library(survival)
library(gridExtra)
library(cowplot)

###Parameters to estimate and other simulation info----------------------
#number of batches (e.g. 100)
number_of_batches = 1200
#number per batch (e.g. 10)
number_per_batch = 10
#check by putting total number here
number_of_iterations = 12000

#path to the directory full of the files
location <- "~/Desktop/mar_2023/safety_test_3"
#"~/Google Drive/My Drive/sim_results/censor_mean_2_sd_1.6_run_16"
#"~/Desktop/Sim_Results/component_mean_run_8_09272022"
#"/Volumes/BN/sim_results_mic.sim/trend_sim_run_9_10212022"

#two formats i have used:
#name_date_number
#name_number_date
format <- "name_date_number"

#general name of simulation array
array_name <- "safety_test_3"
date <- "03072023"


#location <- "~/Desktop/"
#format <- "name_date_number"
#array_name <- "censor_mean_2_sd_0.8_pi2_0.8_run_16"
#date <- "01032023"

incomplete <- check_array_complete(number_of_batches = number_of_batches, format = format, location = location, array_name = array_name, date = date)

print(incomplete)


##eventually an ncomp thing here

##get target values from simulation parameter log
parameter_log <- read_excel("~/Desktop/Sim_Results/simulation_parameter_log.xlsx",
                            sheet = "Sheet4")

#thresholds
sigma_tolerance = c(0.05, 50)
pi_tolerance = c(0.05, 0.95)
intercepts_tolerance = 100
trends_tolerance = 100

verbose = 3

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
scale = params %>% pull(scale)

max_it <- params %>% pull(max_it)
ncomp <- params %>% pull(ncomp)
tol_ll <- params %>% pull(tol_ll)
maxiter_survreg <- params %>% pull(maxiter_survreg)
allow_safety <- params %>% pull(allow_safety) %>% as.logical()
cutoff <- params %>% pull(cutoff)



##function to run local sims here to fix the incomplete runs
rerun_incomplete_sets_both(location = location, incomplete = incomplete, number_per_batch = number_per_batch, array_name = array_name, date = date, covariate_effect_vector = covariate_effect_vector, covariate_list = covariate_list, n = n, pi = pi, intercepts = intercepts, trends = trends, sigma = sigma, nyears = nyears, low_con = low_con, high_con = high_con, scale = scale, max_it = max_it, ncomp = ncomp,
                      tol_ll = tol_ll, maxiter_survreg = maxiter_survreg, verbose = verbose, allow_safety = allow_safety, cutoff = cutoff)
##use run_failed_as_support_for_post_script (turn this into a function here)
check_array_complete(number_of_batches = number_of_batches, format = format, location = location, array_name = array_name, date = date)


target_values = tibble(intercepts, trends, sigma, pi, comp = c("c1", "c2")) %>%
  pivot_longer(cols = intercepts:pi, names_to = "parameter", values_to = "true_value")

array_results <-
  purrr::map(
    1:number_of_batches,
    ~ capture_error_measures_one_batch_safety(
      location = location,
      format = format,
      array_name = array_name,
      date = date,
      i = .x,
      batch_size = number_per_batch,
      intercepts = intercepts,
      trends = trends,
      sigma = sigma,
      pi = pi,
      sigma_tolerance = sigma_tolerance,
      pi_tolerance = pi_tolerance,
      intercepts_tolerance = intercepts_tolerance,
      trends_tolerance = trends_tolerance
    )
  ) %>%
  rbindlist() %>% tibble() %>% mutate(
    incorrect_conv = case_when(
      sigma_error == TRUE |
        pi_error == TRUE |
        intercept_error == TRUE | trends_error == TRUE ~  TRUE,
      TRUE ~ FALSE
    )
  ) %>% mutate(
    failure_conv = case_when(
      comp == "Error" ~  TRUE,
      TRUE ~ FALSE
    )
  )
##analyse
array_results %>% group_by(safety_on, fm, fms_called, fms_worked) %>% tally





########From here on out needs revision
failure_to_converge_pct <-
  array_results  %>% filter(comp == "Error") %>% distinct(iter) %>% nrow(.) /
  (number_of_batches * number_per_batch)
failure_to_converge_vector <-
  array_results  %>% filter(comp == "Error") %>% pull(iter) %>% unique() %>% as.numeric()
converge_incorrectly_pct <-
  array_results  %>% filter(
    comp != "Error" &
      (
        sigma_error == TRUE |
          pi_error == TRUE |
          intercept_error == TRUE |
          trends_error == TRUE
      )
  ) %>% distinct(iter) %>% nrow(.) / (number_of_batches * number_per_batch)
converge_incorrectly_vector <-
  array_results  %>% filter(
    comp != "Error" &
      (
        sigma_error == TRUE |
          pi_error == TRUE |
          intercept_error == TRUE |
          trends_error == TRUE
      )
  ) %>% pull(iter) %>% unique() %>% as.numeric()







####Write the functions needed:
local_full_run_function_both <- function(args,
                                    batch_size = 10,
                                    run_name,
                                    n,
                                    t_dist,
                                    pi,
                                    `E[X|T,C]`,
                                    sd_vector,
                                    covariate_list = NULL,
                                    covariate_effect_vector = c(0),
                                    covariate_names = NULL,
                                    low_con,
                                    high_con,
                                    scale = "log",
                                    formula,
                                    max_it = 3000,
                                    ncomp = 2,
                                    tol_ll = 1e-6,
                                    maxiter_survreg = 30,
                                    verbose = 3,
                                    allow_safety = TRUE,
                                    cutoff = 0.9){
  iteration_set <- ((batch_size * args) - (batch_size - 1)):(batch_size * args)

  #run--------
  results <- purrr::map(
    iteration_set,
    ~ full_sim_run_both_cutoffs(
      .x,
      n = n,
      t_dist = t_dist,
      pi = pi,
      `E[X|T,C]` = `E[X|T,C]`,
      sd_vector = sd_vector,
      covariate_list = covariate_list,
      covariate_effect_vector = covariate_effect_vector,
      covariate_names = NULL,
      low_con = low_con,
      high_con = high_con,
      scale = scale,
      formula = formula,
      max_it = max_it,
      ncomp = ncomp,
      tol_ll = tol_ll,
      maxiter_survreg = maxiter_survreg,
      verbose = verbose,
      allow_safety = allow_safety,
      cutoff = cutoff
    ))



  ##add a save here



  file_name <- paste(run_name, args, sep = "_")
  path <- paste0(file_name, ".Rdata")

  save(results, file = path)
}



rerun_incomplete_sets_both <-
  function(location,
           incomplete,
           number_per_batch,
           array_name,
           date,
           covariate_effect_vector,
           covariate_list,
           n,
           pi,
           intercepts,
           trends,
           sigma,
           nyears,
           low_con,
           high_con,
           scale,
           max_it = 3000,
           ncomp = 2,
           tol_ll = 1e-6,
           maxiter_survreg = 30,
           verbose = 3,
           allow_safety = TRUE,
           cutoff = 0.9) {
    if(!is.data.frame(incomplete) && incomplete == "All Clear"){print("No reruns needed, skipping to next step")} else{
      Sys.setlocale (locale = "en_US.UTF-8")
      if(!identical(sort(c("10", "1:")), c("1:", "10"))){
        errorCondition("sort error")
      }

      setwd(location)


      #command line arguments------------
      args <- incomplete %>% pull(incomplete) %>% as.vector()
      batch_size <- number_per_batch
      #batch size: 10, so set the subtracted term to be "batch size - 1"
      #parameters-------

      #this set of runs will vary the sd of the upper component and push it closer to the highest tested concentration (2^2)

      run_name <- paste(array_name, date, sep = "_")
      covariate_effect_vector <- covariate_effect_vector #0 at start is intercept, then add in the desired coefficients for the covariates
      covariate_list <-  covariate_list
      covariate_names <- NULL
      n=n

      pi1 = function(t) {z <- pi[1] #changed to 0.5
      c("1" = z, "2" = 1- z)}

      `E[X|T,C]` = function(t, c)
      {
        case_when(
          c == "1" ~ intercepts[1] + trends[1] * t,
          c == "2" ~ intercepts[2] + trends[2] * t,
          TRUE ~ NaN
        )
      }

      t_dist1 = function(n){runif(n, min = 0, max = nyears)}

      sd_vector = c("1" = sigma[1], "2" = sigma[2]) #0.5, 0.75, 1, 1.25

      low_con = low_con
      high_con = high_con #errored out when this was 2^3
      #RUN 1 : 2
      #RUN 2: 3
      #RUN 3: 4

      scale = "log"

      formula = Surv(time = left_bound,
                     time2 = right_bound,
                     type = "interval2") ~ 0 + c + strata(c) + t:c

      for(i in args){local_full_run_function_both(
        args = i,
        batch_size = batch_size,
        run_name = run_name,
        n = n,
        t_dist = t_dist1,
        pi = pi1,
        `E[X|T,C]` = `E[X|T,C]`,
        sd_vector = sd_vector,
        covariate_list = NULL,
        covariate_effect_vector = c(0),
        covariate_names = NULL,
        low_con = low_con,
        high_con = high_con,
        scale = scale,
        formula = formula,
        max_it = max_it,
        ncomp = ncomp,
        tol_ll = tol_ll,
        maxiter_survreg = maxiter_survreg,
        verbose = verbose,
        allow_safety = allow_safety,
        cutoff = cutoff
      )
      }

      setwd(
        "~/Desktop/Dissertation Project/Chapter 1/mic.sim"
      )
    }

  }
























capture_error_measures_one_run_both_directions_safety <- function(individual_run,
                                                           intercepts,
                                                           trends,
                                                           sigma,
                                                           pi,
                                                           sigma_tolerance = c(.05, 100),
                                                           pi_tolerance = c(.05, .95),
                                                           intercepts_tolerance = 100,
                                                           trends_tolerance = 100 ){

  x <- length(individual_run)

if(individual_run[[x-1]] == "fm_worked" | (individual_run[[x-1]] == "fm_failed_cutoff" & individual_run[[x]] == "fms_not_called")){

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
      num_it = individual_run$steps,
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


} else if(individual_run[[x-1]] == "fm_failed" & individual_run[[x]] == "fms_not_called"){


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
      pi_error = "TRUE",
      intercept_error = "TRUE",
      trends_error = "TRUE",
      survreg_failure_last = TRUE,
      survreg_failure_any = TRUE,
      num_it = NA_integer_,
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
    est_pi = individual_run[[3]]$`P(C = c)`[1],
    true_pi = pi[1]) %>%
    mutate(
      error_intercepts = true_intercepts - est_intercepts,
      error_trends = true_trends - est_trends,
      error_sigma = true_sigma - est_sigma,
      error_pi = true_pi - est_pi
    ) %>%
    tidyr::pivot_longer(cols = est_intercepts:error_pi) %>%
    separate(name, sep = "_", into = c("type", "parameter")) %>%
    pivot_wider(names_from = type, values_from = value) %>%
    mutate(iter = individual_run[[x - 4]],
           steps = nrow(individual_run[[1]]))



  a <- df %>% filter(parameter == "sigma")
  if((abs(a$est) > max(sigma_tolerance)) | (abs(a$est) < min(sigma_tolerance)) ){sigma_error = TRUE
  } else{sigma_error = FALSE}

  a <- df %>% filter(parameter == "pi")
  if(a$est >= max(pi_tolerance) | a$est <= min(pi_tolerance)){pi_error = TRUE
  } else{pi_error = FALSE}

  a <- df %>% filter(parameter == "intercepts")
  if(abs(a$est) >= intercepts_tolerance){intercept_error = TRUE
  } else{intercept_error = FALSE}

  a <- df %>% filter(parameter == "trends")
  if(abs(a$est) >= trends_tolerance){trends_error = TRUE
  } else{trends_error = FALSE}




  df2 <-
    df %>% mutate(
      sigma_error = sigma_error,
      pi_error = pi_error,
      intercept_error = intercept_error,
      trends_error = trends_error,
      survreg_failure_last = sr_last,
      survreg_failure_any = sr_any,
      num_it = individual_run$steps,
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

  } else if(individual_run[[x-1]] ==  %in% c("fm_failed", "fm_failed_cutoff") & individual_run[[x]] == "fms_failed"){


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
        pi_error = "TRUE",
        intercept_error = "TRUE",
        trends_error = "TRUE",
        survreg_failure_last = TRUE,
        survreg_failure_any = TRUE,
        num_it = NA_integer_,
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


capture_error_measures_one_batch_safety <- function(location,
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
  results <- loadRData(file)
  # results_pre <- sticky::sticky(results_pre)

  #attr_print  <- function(run){
  #    print(attr(run, "survreg_failure"))
  #}

  #results_pre_attr <- map(results_pre, attr_append)

  #results <- purrr::map2(1:batch_size, results_pre, ~append(.y, (batch_size*(i - 1) + .x )))

  #note results[[3]] failure is not list object

  purrr::map(results, ~capture_error_measures_one_run_both_directions_safety(.x, intercepts, trends, sigma, pi, sigma_tolerance, pi_tolerance, intercepts_tolerance, trends_tolerance)) %>%
    data.table::rbindlist()
  ##add attribute read to the error_measures_one_run_both_directions segment

}

#attr_append <- function(run){
#  append(run, attr(run, "survreg_failure"))
#}


