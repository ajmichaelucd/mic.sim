#libraries-------
#rm(list = ls())
library(magrittr)
library(dplyr)
library(tidyr)
load_all()
library(mic.sim)
library(ggplot2)
library(LearnBayes)
library(survival)
library(biglm)
library(gridExtra)
library(data.table)

#parameters-------
run_name
iterations <- 1
covariate_effect_vector <- c(0) #0 at start is intercept, then add in the desired coefficients for the covariates
covariate_list <-  NULL
covariate_names <- NULL
n=2000
ncomp = 2
pi1 = function(t) {z <- 0.6
c("1" = z, "2" = 1- z)}

`E[X|T,C]` = function(t, c)
{
  case_when(
    c == "1" ~ 0 + 0.05 * t,
    c == "2" ~ 3 + 0 * t,
    TRUE ~ NaN
  )
}

t_dist1 = function(n){runif(n, min = 0, max = 5)}

sd_vector = c("1" = 1, "2" = 1)

low_con = 2^-2
high_con = 2^3

scale = "log"

formula = Surv(time = left_bound,
               time2 = right_bound,
               type = "interval2") ~ 0 + c + strata(c) + t:c
max_it = 3000
ncomp = 2
tol_ll = 1e-6

#run--------
results <- purrr::map(
  12,
  ~ full_sim_in_1_function(
    .x,
    n = n,
    t_dist = t_dist1,
    pi = pi1,
    `E[X|T,C]` = `E[X|T,C]`,
    sd_vector = sd_vector,
    covariate_list = covariate_list,
    covariate_effect_vector = covariate_effect_vector,
    low_con = low_con,
    high_con = high_con,
    scale = scale,
    formula = formula,
    max_it = max_it,
    ncomp = ncomp,
    tol_ll = tol_ll
  )
)



#error metrics------



post_run_analysis_2 <- function(results, intercepts, trends, sigma){
  a <- min_rank(abs(intercepts - results[[4]]$mean[1]))
  b <- min_rank(abs(intercepts - results[[4]]$mean[2]))
  errorCondition(rev(b) != a, "Issue with identifying means")

  tibble(
    comp = c("c1", "c2"),
    est_intercepts = results[[4]]$mean[a],
    true_intercepts = intercepts,
    est_trends = results[[4]]$mean[a + 2],
    true_trends = trends,
    est_sigma = results[[4]]$sd[a],
    true_sigma = sigma) %>%
    mutate(
      error_intercepts = true_intercepts - est_intercepts,
      error_trends = true_trends - est_trends,
      error_sigma = true_sigma - est_sigma
    ) %>%
    tidyr::pivot_longer(cols = est_intercepts:error_sigma) %>%
    separate(name, sep = "_", into = c("type", "parameter")) %>%
    pivot_wider(names_from = type, values_from = value)




  #c1-c2 format
  #c2-c1 format
}

errors <- purrr::map( results, ~post_run_analysis_2(.x,
                    intercepts = c( 0, 2),
                    trends = c(-0.4, 0.2),
                    sigma = c(1, 1))) %>%
data.table::rbindlist() %>% tibble() %>%
  mutate(iteration = rep(1:length(results), each =  nrow(.) / length(results)))

errors %>%
  group_by(comp, parameter) %>%
  summarize(mean_est = mean(est),
            std_error = sd(est) / sqrt(length(results)),
            bias = mean(error),
            MSE = mean(error^2)
  ) -> summary_stats1

#scratch_work------

#
#
#
#
#
#a <- min_rank(abs(intercepts - results[[10]][[4]]$mean[1]))
#b <- min_rank(abs(intercepts - results[[10]][[4]]$mean[2]))
#errorCondition(rev(b) != a, "Issue with identifying means")
#
#tibble(
#        comp = c("c1", "c2"),
#        est_intercepts = results[[10]][[4]]$mean[a],
#        true_intercepts = intercepts,
#        est_trends = results[[10]][[4]]$mean[a + 2],
#        true_trends = trends,
#        est_sigma = results[[10]][[4]]$sd[a],
#        true_sigma = sigma) %>%
#  mutate(
#    error_intercepts = true_intercepts - est_intercepts,
#    error_trends = true_trends - est_trends,
#    error_sigma = true_sigma - est_sigma
#  ) %>%
#  tidyr::pivot_longer(cols = est_intercepts:error_sigma) %>%
#  separate(name, sep = "_", into = c("type", "parameter")) %>%
#  pivot_wider(names_from = type, values_from = value)
#
#
#


list(summary_stats1, `E[X|T,C]`, pi1, sd_vector) %>%
  save(fs::path("misc", run_name, ext = "Rdata"))





