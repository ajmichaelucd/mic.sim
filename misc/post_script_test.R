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
location <- "~/Google Drive/My Drive/sim_results/censor_mean_2_sd_1_pi2_0.8/censor_mean_2_sd_1_pi2_0.8_run_11"
  #"~/Google Drive/My Drive/sim_results/censor_mean_2_sd_1.6_run_16"
  #"~/Desktop/Sim_Results/component_mean_run_8_09272022"
  #"/Volumes/BN/sim_results_mic.sim/trend_sim_run_9_10212022"

#two formats i have used:
#name_date_number
#name_number_date
format <- "name_date_number"

#general name of simulation array
array_name <- "censor_mean_2_sd_1_pi2_0.8_run_11"
date <- "01032023"

incomplete <- check_array_complete(number_of_batches = number_of_batches, format = format, location = location, array_name = array_name, date = date)

print(incomplete)


##eventually an ncomp thing here

##get target values from simulation parameter log
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

##function to run local sims here to fix the incomplete runs
rerun_incomplete_sets(location = location, incomplete = incomplete, number_per_batch = number_per_batch, array_name = array_name, date = date, covariate_effect_vector = covariate_effect_vector, covariate_list = covariate_list, n = n, pi = pi, intercepts = intercepts, trends = trends, sigma = sigma, nyears = nyears, low_con = low_con, high_con = high_con)
##use run_failed_as_support_for_post_script (turn this into a function here)
check_array_complete(number_of_batches = number_of_batches, format = format, location = location, array_name = array_name, date = date)

###Load in data and extract information from it------------
array_results <- purrr::map(1:number_of_batches, ~error_measures_one_batch(location = location, format = format, array_name = array_name, date = date, i = .x, batch_size = number_per_batch, intercepts = intercepts, trends = trends, sigma = sigma, pi = pi, sigma_tolerance = sigma_tolerance, pi_tolerance = pi_tolerance, intercepts_tolerance = intercepts_tolerance, trends_tolerance = trends_tolerance))
failure_to_converge_pct <- (array_results %>% rbindlist() %>% tibble() %>% filter(comp == "Error") %>% summarize(n = n() / (number_of_batches * number_per_batch)))
failure_to_converge_vector <- array_results %>% rbindlist() %>% tibble() %>% filter(comp == "Error") %>% pull(iter) %>% as.numeric()
converge_incorrectly_pct <- (array_results %>% rbindlist() %>% tibble() %>% filter(comp != "Error" & (sigma_error == TRUE | pi_error == TRUE | intercept_error == TRUE | trends_error == TRUE)) %>% group_by(iter) %>% summarise(n = 1 / (number_of_batches * number_per_batch)) %>% summarise(n = sum(n)))
converge_incorrectly_vector <- array_results %>% rbindlist() %>% tibble() %>% filter(comp != "Error" & (sigma_error == TRUE | pi_error == TRUE | intercept_error == TRUE | trends_error == TRUE)) %>% pull(iter) %>% unique() %>% as.numeric()
array_results %>% rbindlist() %>% tibble() %>% filter(comp != "Error" & sigma_error == FALSE & pi_error == FALSE & intercept_error == FALSE & trends_error == FALSE) %>% mutate_at(c('est', 'true', 'error', 'iter'), as.numeric) %>% group_by(comp, parameter) %>%
  summarize(mean_est = mean(est),
            std_error = sd(est),
            bias = mean(error),
            MSE = mean(error^2)
  )

report_failure_types(array_results)

tibble(failed_convergence = failure_to_converge_pct$n, incorrect_convergence = converge_incorrectly_pct$n, total_failure_proportion = failure_to_converge_pct$n + converge_incorrectly_pct$n)

failure_to_converge_vector

results_tibble <- array_results %>% rbindlist() %>% tibble() %>% filter(comp != "Error" & sigma_error == FALSE & pi_error == FALSE & intercept_error == FALSE & trends_error == FALSE) %>% mutate_at(c('est', 'true', 'error', 'iter'), as.numeric)
#include incorrect, add tags for converge failure/success, and add to plots below



#intercepts
#trends
#sigma

df2 <- purrr::map(1:number_of_iterations, ~describe_data_set(i = .x, n = n, intercepts = intercepts, trends = trends, sigma = sigma, pi = pi, nyears = nyears, converge_incorrectly_vector = converge_incorrectly_vector, failure_to_converge_vector = failure_to_converge_vector, scale = scale))
df2 %>% rbindlist() %>% tibble() %>%
  group_by(convergence, comp) %>%
  summarise(t = mean(t_avg),
            eps = mean(eps_avg),
            obs_val = mean(obs_val_avg),
            obs_val_adj = mean(obs_val_avg_adj),
            censored_pct = mean(censored_pct),
            intercept = mean(intercepts),
            trend = mean(trends)

  )



###Plot original data sets for a few iterations------------
set_numbers = c(10, 22, 2, 7, 1, 3)


recreate_and_plot_data(set_numbers = set_numbers, n = n, intercepts = intercepts, trends = trends, sigma = sigma, pi = pi, nyears = nyears, covariate_list = covariate_list, covariate_effect_vector = covariate_effect_vector, low_con = low_con, high_con = high_con, scale = scale, converge_incorrectly_vector = converge_incorrectly_vector, failure_to_converge_vector = failure_to_converge_vector)




###Plot parameter estimates-----------

c2_intercepts_plot <- results_tibble %>%
  filter(parameter == "intercepts") %>%
  filter(comp == "c2") %>%
#  mutate(error2 = cut(abs(error), breaks = seq(from = 0, to = 3, by = 0.2))) %>%
  ggplot(aes(x = est)) +
  geom_histogram( binwidth = 0.05, fill = "darkgreen", color = "black") +
  geom_vline(aes(xintercept = true), color = "red") +
  #scale_x_continuous(limits = c(-3.2, 3.2), oob = scales::squish, breaks = c(-3: 3)
  ggtitle(label = "Component 2 Intercept")
#+ geom_text(stat='count', aes(label=..count..), vjust = -10)

c1_intercepts_plot <- results_tibble %>%
  filter(parameter == "intercepts") %>%
  filter(comp == "c1") %>%
  #  mutate(error2 = cut(abs(error), breaks = seq(from = 0, to = 3, by = 0.2))) %>%
  ggplot(aes(x = est)) +
  geom_histogram( binwidth = 0.05, fill = "darkgreen", color = "black") +
  geom_vline(aes(xintercept = true), color = "red") +
  #scale_x_continuous(limits = c(-3.2, 3.2), oob = scales::squish, breaks = c(-3: 3)
  #) +
  ggtitle(label = "Component 1 Intercept")


c2_trends_plot <- results_tibble %>%
  filter(parameter == "trends") %>%
  filter(comp == "c2") %>%
  #  mutate(error2 = cut(abs(error), breaks = seq(from = 0, to = 3, by = 0.2))) %>%
  ggplot(aes(x = est)) +
  geom_histogram(
    binwidth = 0.01,
    fill = "darkgreen", color = "black") +
  geom_vline(aes(xintercept = true), color = "red") +
  #scale_x_continuous(limits = c(-.52, .52), oob = scales::squish, breaks = c(seq(-.5, .5, by = .1))) +
  ggtitle(label = "Component 2 Slope")


c1_trends_plot <- results_tibble %>%
  filter(parameter == "trends") %>%
  filter(comp == "c1") %>%
  #  mutate(error2 = cut(abs(error), breaks = seq(from = 0, to = 3, by = 0.2))) %>%
  ggplot(aes(x = est)) +
  geom_histogram(
    binwidth = 0.01,
    fill = "darkgreen", color = "black") +
  geom_vline(aes(xintercept = true), color = "red") +
  #scale_x_continuous(limits = c(-.52, .52), oob = scales::squish, breaks = c(seq(-.5, .5, by = .1)) ) +
  ggtitle(label = "Component 1 Slope")

c1_sigma_plot <- results_tibble %>%
  filter(parameter == "sigma") %>%
  filter(comp == "c1") %>%
  #  mutate(error2 = cut(abs(error), breaks = seq(from = 0, to = 3, by = 0.2))) %>%
  ggplot(aes(x = est)) +
  geom_histogram(
    #binwidth = 0.02,
    fill = "darkgreen", color = "black") +
  geom_vline(aes(xintercept = true), color = "red") +
  #scale_x_continuous(limits = c(-.52, .52), oob = scales::squish, breaks = c(seq(-.5, .5, by = .1)) ) +
  ggtitle(label = "Component 1 Sigma")

c2_sigma_plot <- results_tibble %>%
  filter(parameter == "sigma") %>%
  filter(comp == "c2") %>%
  #  mutate(error2 = cut(abs(error), breaks = seq(from = 0, to = 3, by = 0.2))) %>%
  ggplot(aes(x = est)) +
  geom_histogram(
    #binwidth = 0.02,
    fill = "darkgreen", color = "black") +
  geom_vline(aes(xintercept = true), color = "red") +
  #scale_x_continuous(limits = c(-.52, .52), oob = scales::squish, breaks = c(seq(-.5, .5, by = .1)) ) +
  ggtitle(label = "Component 2 Sigma")

c1_pi_plot <- results_tibble %>%
  filter(parameter == "pi") %>%
  filter(comp == "c1") %>%
  #  mutate(error2 = cut(abs(error), breaks = seq(from = 0, to = 3, by = 0.2))) %>%
  ggplot(aes(x = est)) +
  geom_histogram(
    #binwidth = 0.02,
    fill = "darkgreen", color = "black") +
  geom_vline(aes(xintercept = true), color = "red") +
  #scale_x_continuous(limits = c(-.52, .52), oob = scales::squish, breaks = c(seq(-.5, .5, by = .1)) ) +
  ggtitle(label = "Component 1 Pi")

# c2_pi_plot <- results_tibble %>%
#   filter(parameter == "pi") %>%
#   filter(comp == "c2") %>%
#   #  mutate(error2 = cut(abs(error), breaks = seq(from = 0, to = 3, by = 0.2))) %>%
#   ggplot(aes(x = est)) +
#   geom_histogram(
#     #binwidth = 0.02,
#     fill = "darkgreen", color = "black") +
#   geom_vline(aes(xintercept = true), color = "red") +
#   #scale_x_continuous(limits = c(-.52, .52), oob = scales::squish, breaks = c(seq(-.5, .5, by = .1)) ) +
#   ggtitle(label = "Component 2 Pi")

steps_plot <- array_results %>% rbindlist() %>% tibble() %>% filter(comp != "Error") %>% mutate(Error = ifelse(sigma_error == FALSE & pi_error == FALSE & intercept_error == FALSE & trends_error == FALSE, FALSE, TRUE)) %>%
  select(iter:Error) %>%
  ggplot(aes(x = steps)) +
  geom_histogram(aes(fill = Error)) + ggtitle(label = "Steps")



library(cowplot)
ggdraw() +
  draw_plot(c1_intercepts_plot, x = 0, y = .75, width = .5, height = .25)  +
  draw_plot(c2_intercepts_plot, x = 0.5, y = .75, width = .5, height = .25)  +
  draw_plot(c1_sigma_plot, x = 0, y = .5, width = .5, height = .25)  +
  draw_plot(c2_sigma_plot, x = 0.5, y = .5, width = .5, height = .25)  +
  draw_plot(c1_trends_plot, x = 0, y = .25, width = .5, height = .25)  +
  draw_plot(c2_trends_plot, x = 0.5, y = .25, width = .5, height = .25)  +
  draw_plot(c1_pi_plot, x = 0, y = .0, width = .5, height = .25)  +
  draw_plot(steps_plot, x = 0.5, y = .0, width = .5, height = .25)








