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
location <- "~/Desktop/mar_2023/safety_test_4"
#"~/Google Drive/My Drive/sim_results/censor_mean_2_sd_1.6_run_16"
#"~/Desktop/Sim_Results/component_mean_run_8_09272022"
#"/Volumes/BN/sim_results_mic.sim/trend_sim_run_9_10212022"

#two formats i have used:
#name_date_number
#name_number_date
format <- "name_date_number"

#general name of simulation array
array_name <- "safety_test_4"
date <- "03212023"


#location <- "~/Desktop/"
#format <- "name_date_number"
#array_name <- "censor_mean_2_sd_0.8_pi2_0.8_run_16"
#date <- "01032023"

incomplete <- check_array_complete(number_of_batches = number_of_batches, format = format, location = location, array_name = array_name, date = date)

print(incomplete)


##eventually an ncomp thing here

##get target values from simulation parameter log
parameter_log <- read_excel("~/Desktop/Sim_Results/simulation_parameter_log.xlsx",
                            sheet = "Sheet5")

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
pi_int = c(params$`C1 Pi intercept`)  #c(0.5, 0.5)
pi_trend = c(params$pi_trend)
low_con = params$low_con
high_con = params$high_con
formula = Surv(time = left_bound,
               time2 = right_bound,
               type = "interval2") ~ as.formula(params$formula)
formula2 = as.formula(params$formula2)
nyears = params$years
n = params$n
scale = params %>% pull(scale)

fms_only <- params %>% pull(fms_only)
initial_weighting	<- params %>% pull(initial_weighting)
pi_function	<- params %>% pull(pi_function)
pi_link	<- params %>% pull(pi_link)
pi_truth <- params %>% pull(pi_truth)

max_it <- params %>% pull(max_it)
ncomp <- params %>% pull(ncomp)
tol_ll <- params %>% pull(tol_ll)
maxiter_survreg <- params %>% pull(maxiter_survreg)
allow_safety <- params %>% pull(allow_safety) %>% as.logical()
cutoff <- params %>% pull(cutoff)

######IMPLEMENT A CHECK HERE VS FIRST AVAILABLE DATA SET

##function to run local sims here to fix the incomplete runs
rerun_incomplete_sets(
  location = location,
  incomplete = incomplete,
  number_per_batch = number_per_batch,
  array_name = array_name,
  date = date,
  covariate_effect_vector = covariate_effect_vector,
  covariate_list = covariate_list,
  n = n,
  pi_int = pi_int,
  pi_trend = pi_trend,
  intercepts = intercepts,
  trends = trends,
  sigma = sigma,
  nyears = nyears,
  low_con = low_con,
  high_con = high_con,
  formula = formula,
  formula2 = formula2,
  scale = scale,
  max_it = max_it,
  ncomp = ncomp,
  tol_ll = tol_ll,
  maxiter_survreg = maxiter_survreg,
  verbose = verbose,
  allow_safety = allow_safety,
  cutoff = cutoff,
  fms_only = fms_only,
  initial_weighting  = initial_weighting,
  pi_function = pi_function,
  pi_link = pi_link,
  pi_truth = pi_truth
)
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
      pi_int = pi_int,
      pi_trend = pi_trend,
      sigma_tolerance = sigma_tolerance,
      pi_tolerance = pi_tolerance,
      intercepts_tolerance = intercepts_tolerance,
      trends_tolerance = trends_tolerance,
      number_of_batches = number_of_batches
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
















