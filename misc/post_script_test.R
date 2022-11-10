#libraries
load_all()
library(dplyr)
library(tidyr)
library(purrr)
library(data.table)
library(fs)
library(ggplot2)
library(stats)
library(magrittr)



#number of batches (e.g. 100)
number_of_batches = 100
#number per batch (e.g. 10)
number_per_batch = 10
#check by putting total number here
number_of_iterations = 1000

#path to the directory full of the files
location <- "~/Desktop/Sim_Results/component_mean_run_8_09272022"  #"/Volumes/BN/sim_results_mic.sim/trend_sim_run_9_10212022"

#two formats i have used:
#name_date_number
#name_number_date
format <- "name_date_number"

#general name of simulation array
array_name <- "component_mean_run_8"
date <- "09272022"

check_array_complete(number_of_batches = number_of_batches, format = format, location = location, array_name = array_name, date = date)


#target values
intercepts = c(-1, 2.2)
trends = c(0.1, 0)
sigma = c(1, 0.5)
pi = c(0.5, 0.5)

nyears = 5

#thresholds
sigma_tolerance = c(0.25, 50)
pi_tolerance = c(0.05, 0.95)
intercepts_tolerance = 100
trends_tolerance = 100


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

df2 <- purrr::map(1:number_of_iterations, ~describe_data_set(i = .x, n = n, intercepts = intercepts, trends = trends, sigma = sigma, pi = pi, nyears = nyears, converge_incorrectly_vector = converge_incorrectly_vector, failure_to_converge_vector = failure_to_converge_vector))
df2 %>% rbindlist() %>% tibble() %>%
  group_by(convergence, comp) %>%
  summarise(t = mean(t_avg),
            eps = mean(eps_avg),
            obs_val = mean(obs_val_avg),
            obs_val_adj = mean(obs_val_avg_adj),
            censored_pct = mean(censored_pct)

  )




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








