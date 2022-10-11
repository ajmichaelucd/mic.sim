#libraries
library(dplyr)
library(tidyr)
library(purrr)
library(data.table)
library(fs)

#number of batches (e.g. 100)
number_of_batches = 100
#number per batch (e.g. 10)
number_per_batch = 10
#check by putting total number here
number_of_iterations = 1000

#path to the directory full of the files
location <- "~/Desktop/Sim_Results/component_mean_run_3_09272022/"

#two formats i have used:
#name_date_number
#name_number_date
format <- "name_date_number"

#general name of simulation array
array_name <- "component_mean_run_3"
date <- "09272022"





#target values
intercepts = c(-1, 1.75)
trends = c(0.1, 0)
sigma = c(1, 0.5)
pi = c(0.5, 0.5)

#thresholds
sigma_tolerance = 100
pi_tolerance = 0.95
intercepts_tolerance = 100
trends_tolerance = 100





#load("~/Desktop/small_trend_run_1_09192022/small_trend_run_1_09192022_35.Rdata")






#need a method for when result is "Error"









array_results <- purrr::map(1:number_of_batches, ~error_measures_one_batch(location = location, format = format, array_name = array_name, date = date, i = .x, intercepts = intercepts, trends = trends, sigma = sigma, pi = pi, sigma_tolerance = sigma_tolerance, pi_tolerance = pi_tolerance, intercepts_tolerance = intercepts_tolerance, trends_tolerance = trends_tolerance))
failure_to_converge_pct <- (array_results %>% rbindlist() %>% tibble() %>% filter(comp == "Error") %>% summarize(n = n() / (number_of_batches * number_per_batch)))
converge_incorrectly_pct <- (array_results %>% rbindlist() %>% tibble() %>% filter(comp != "Error" & (sigma_error == TRUE | pi_error == TRUE | intercept_error == TRUE | trends_error == TRUE)) %>% group_by(iter) %>% summarise(n = 1 / (number_of_batches * number_per_batch)) %>% summarise(n = sum(n)))
array_results %>% rbindlist() %>% tibble() %>% filter(comp != "Error" & sigma_error == FALSE & pi_error == FALSE & intercept_error == FALSE & trends_error == FALSE) %>% mutate_at(c('est', 'true', 'error', 'iter'), as.numeric) %>% group_by(comp, parameter) %>%
  summarize(mean_est = mean(est),
            std_error = sd(est),
            bias = mean(error),
            MSE = mean(error^2)
  )

results_tibble <- array_results %>% rbindlist() %>% tibble() %>% filter(comp != "Error" & sigma_error == FALSE & pi_error == FALSE & intercept_error == FALSE & trends_error == FALSE) %>% mutate_at(c('est', 'true', 'error', 'iter'), as.numeric)

#intercepts
#trends
#sigma

library(ggplot2)


results_tibble %>%
  filter(parameter == "intercepts") %>%
  filter(comp == "c2") %>%
#  mutate(error2 = cut(abs(error), breaks = seq(from = 0, to = 3, by = 0.2))) %>%
  ggplot(aes(x = error)) +
  geom_histogram( binwidth = 0.05, fill = "darkgreen", color = "black") +
  geom_vline(aes(xintercept = 0), color = "red") +
  #scale_x_continuous(limits = c(-3.2, 3.2), oob = scales::squish, breaks = c(-3: 3)
  ggtitle(label = "Component 2 Intercept")
#+ geom_text(stat='count', aes(label=..count..), vjust = -10)

results_tibble %>%
  filter(parameter == "intercepts") %>%
  filter(comp == "c1") %>%
  #  mutate(error2 = cut(abs(error), breaks = seq(from = 0, to = 3, by = 0.2))) %>%
  ggplot(aes(x = error)) +
  geom_histogram( binwidth = 0.05, fill = "darkgreen", color = "black") +
  geom_vline(aes(xintercept = 0), color = "red") +
  #scale_x_continuous(limits = c(-3.2, 3.2), oob = scales::squish, breaks = c(-3: 3)
  #) +
  ggtitle(label = "Component 1 Intercept")


results_tibble %>%
  filter(parameter == "trends") %>%
  filter(comp == "c2") %>%
  #  mutate(error2 = cut(abs(error), breaks = seq(from = 0, to = 3, by = 0.2))) %>%
  ggplot(aes(x = error)) +
  geom_histogram(
    binwidth = 0.01,
    fill = "darkgreen", color = "black") +
  geom_vline(aes(xintercept = 0), color = "red") +
  #scale_x_continuous(limits = c(-.52, .52), oob = scales::squish, breaks = c(seq(-.5, .5, by = .1))) +
  ggtitle(label = "Component 2 Slope")


results_tibble %>%
  filter(parameter == "trends") %>%
  filter(comp == "c1") %>%
  #  mutate(error2 = cut(abs(error), breaks = seq(from = 0, to = 3, by = 0.2))) %>%
  ggplot(aes(x = error)) +
  geom_histogram(
    binwidth = 0.01,
    fill = "darkgreen", color = "black") +
  geom_vline(aes(xintercept = 0), color = "red") +
  #scale_x_continuous(limits = c(-.52, .52), oob = scales::squish, breaks = c(seq(-.5, .5, by = .1)) ) +
  ggtitle(label = "Component 1 Slope")

results_tibble %>%
  filter(parameter == "sigma") %>%
  filter(comp == "c1") %>%
  #  mutate(error2 = cut(abs(error), breaks = seq(from = 0, to = 3, by = 0.2))) %>%
  ggplot(aes(x = error)) +
  geom_histogram(
    #binwidth = 0.02,
    fill = "darkgreen", color = "black") +
  geom_vline(aes(xintercept = 0), color = "red") +
  #scale_x_continuous(limits = c(-.52, .52), oob = scales::squish, breaks = c(seq(-.5, .5, by = .1)) ) +
  ggtitle(label = "Component 1 Sigma")

results_tibble %>%
  filter(parameter == "sigma") %>%
  filter(comp == "c2") %>%
  #  mutate(error2 = cut(abs(error), breaks = seq(from = 0, to = 3, by = 0.2))) %>%
  ggplot(aes(x = error)) +
  geom_histogram(
    #binwidth = 0.02,
    fill = "darkgreen", color = "black") +
  geom_vline(aes(xintercept = 0), color = "red") +
  #scale_x_continuous(limits = c(-.52, .52), oob = scales::squish, breaks = c(seq(-.5, .5, by = .1)) ) +
  ggtitle(label = "Component 2 Sigma")

results_tibble %>%
  filter(parameter == "pi") %>%
  filter(comp == "c1") %>%
  #  mutate(error2 = cut(abs(error), breaks = seq(from = 0, to = 3, by = 0.2))) %>%
  ggplot(aes(x = error)) +
  geom_histogram(
    #binwidth = 0.02,
    fill = "darkgreen", color = "black") +
  geom_vline(aes(xintercept = 0), color = "red") +
  #scale_x_continuous(limits = c(-.52, .52), oob = scales::squish, breaks = c(seq(-.5, .5, by = .1)) ) +
  ggtitle(label = "Component 2 Pi")













