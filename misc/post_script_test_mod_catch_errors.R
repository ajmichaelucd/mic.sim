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


###Parameters to estimate and other simulation info----------------------
#number of batches (e.g. 100)
number_of_batches = 100
#number per batch (e.g. 10)
number_per_batch = 10
#check by putting total number here
number_of_iterations = 1000

#path to the directory full of the files
location <- "~/Desktop/feb_2023/gamithromycin_mh_22"
#"~/Google Drive/My Drive/sim_results/censor_mean_2_sd_1.6_run_16"
#"~/Desktop/Sim_Results/component_mean_run_8_09272022"
#"/Volumes/BN/sim_results_mic.sim/trend_sim_run_9_10212022"

#two formats i have used:
#name_date_number
#name_number_date
format <- "name_date_number"

#general name of simulation array
array_name <- "gamithromycin_mh_22"
date <- "02082023"


#location <- "~/Desktop/"
#format <- "name_date_number"
#array_name <- "censor_mean_2_sd_0.8_pi2_0.8_run_16"
#date <- "01032023"

incomplete <- check_array_complete(number_of_batches = number_of_batches, format = format, location = location, array_name = array_name, date = date)

print(incomplete)


##eventually an ncomp thing here

##get target values from simulation parameter log
parameter_log <- read_excel("~/Desktop/Sim_Results/simulation_parameter_log.xlsx",
                            sheet = "Sheet3")

#thresholds
sigma_tolerance = c(0.05, 50)
pi_tolerance = c(0.05, 0.95)
intercepts_tolerance = 100
trends_tolerance = 100

max_it = 3000
ncomp = 2
tol_ll = 1e-6
maxiter_survreg = 30
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


##function to run local sims here to fix the incomplete runs
rerun_incomplete_sets(location = location, incomplete = incomplete, number_per_batch = number_per_batch, array_name = array_name, date = date, covariate_effect_vector = covariate_effect_vector, covariate_list = covariate_list, n = n, pi = pi, intercepts = intercepts, trends = trends, sigma = sigma, nyears = nyears, low_con = low_con, high_con = high_con, max_it = max_it, ncomp = ncomp,
                      tol_ll = tol_ll, maxiter_survreg = maxiter_survreg, verbose = verbose)
##use run_failed_as_support_for_post_script (turn this into a function here)
check_array_complete(number_of_batches = number_of_batches, format = format, location = location, array_name = array_name, date = date)


target_values = tibble(intercepts, trends, sigma, pi, comp = c("c1", "c2")) %>%
  pivot_longer(cols = intercepts:pi, names_to = "parameter", values_to = "true_value")

array_results <-
  purrr::map(
    1:number_of_batches,
    ~ capture_error_measures_one_batch(
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
survreg_failure_last_pct <-
  array_results  %>% filter(survreg_failure_last == TRUE) %>% distinct(iter) %>% nrow(.) /
  (number_of_batches * number_per_batch)
survreg_failure_last_vector <-
  array_results %>% filter(survreg_failure_last == TRUE) %>% pull(iter) %>% unique() %>% as.numeric()

survreg_failure_any_pct <-
  array_results  %>% filter(survreg_failure_any == TRUE) %>% distinct(iter) %>% nrow(.) /
  (number_of_batches * number_per_batch)
survreg_failure_any_vector <-
  array_results %>% filter(survreg_failure_any == TRUE) %>% pull(iter) %>% unique() %>% as.numeric()




success_pct <-  array_results  %>% filter(survreg_failure_last == FALSE & incorrect_conv == FALSE & failure_conv == FALSE) %>% distinct(iter) %>% nrow(.) /
  (number_of_batches * number_per_batch)
success_vector <-  array_results  %>% filter(survreg_failure_last == FALSE & incorrect_conv == FALSE & failure_conv == FALSE) %>% pull(iter) %>% unique() %>% as.numeric()


array_results %>% group_by(iter, survreg_failure_last, incorrect_conv, failure_conv) %>% summarise(n = 1) %>% group_by(survreg_failure_last, incorrect_conv, failure_conv) %>% summarize(n = n())

array_results %>% group_by(iter, survreg_failure_any, survreg_failure_last) %>% summarise(n = 1) %>% group_by(survreg_failure_any, survreg_failure_last) %>% tally

##Summary for: converged successfully and not extremely incorrectly
array_results %>% filter(comp != "Error" & sigma_error == FALSE & pi_error == FALSE & intercept_error == FALSE & trends_error == FALSE) %>% mutate_at(c('est', 'true', 'error', 'iter'), as.numeric) %>% group_by(comp, parameter) %>%
  summarize(mean_est = mean(est),
            median_est = median(est),
            bias = mean(error),
            med_bias = median(error),
            std_error = sd(est),
            MSE = mean(error^2)
  ) %>% left_join(., target_values) %>%
  relocate(true_value, .after = median_est)

##Summary for: converged successfully, not extremely incorrectly, survreg did not run out of iterations
array_results %>% filter(comp != "Error" & incorrect_conv == FALSE & survreg_failure_last == FALSE) %>% mutate_at(c('est', 'true', 'error', 'iter'), as.numeric) %>% group_by(comp, parameter) %>%
  summarize(mean_est = mean(est),
            median_est = median(est),
            bias = mean(error),
            med_bias = median(error),
            std_error = sd(est),
            MSE = mean(error^2)
  ) %>% left_join(., target_values) %>%
  relocate(true_value, .after = median_est)

##Maybe one here where the survregs fail and one where survreg doesn't?:
array_results %>% filter(comp != "Error" & survreg_failure == FALSE) %>% mutate_at(c('est', 'true', 'error', 'iter'), as.numeric) %>% group_by(comp, parameter) %>%
  summarize(mean_est = mean(est),
            median_est = median(est),
            bias = mean(error),
            med_bias = median(error),
            std_error = sd(est),
            MSE = mean(error^2)
  ) %>% left_join(., target_values) %>%
  relocate(true_value, .after = median_est)


array_results %>% filter(comp != "Error" & survreg_failure == TRUE) %>% mutate_at(c('est', 'true', 'error', 'iter'), as.numeric) %>% group_by(comp, parameter) %>%
  summarize(mean_est = mean(est),
            median_est = median(est),
            bias = mean(error),
            med_bias = median(error),
            std_error = sd(est),
            MSE = mean(error^2)
  ) %>% left_join(., target_values) %>%
  relocate(true_value, .after = median_est)






###DOES NOT WORK, NEED TO MODIFY SINCE ARRAY RESULTS ISNT A LIST HERE ANY MORE
##report_failure_types(array_results)

tibble(failed_convergence = failure_to_converge_pct, incorrect_convergence = converge_incorrectly_pct, survreg_failure = survreg_failure_pct,total_failure_proportion = 1 - success_pct)


##overlap and separated vectors here maybe?











