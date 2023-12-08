Sys.setlocale (locale = "en_US.UTF-8")
print(sort(c("10", "1:")))

library(magrittr)
library(dplyr)
library(tidyr)
library(mic.sim)
library(survival)
library(gridExtra)
library(data.table)
library(purrr)
library(stringr)
library(mgcv)

args <- as.numeric(commandArgs(trailingOnly = TRUE))

#things to rotate
  #pi
  #mu
  #high_con
  #low_con
  #sd_vector

sd_vals = list(
  c(`1` = 0.8, `2` = 0.6),
  c(`1` = 0.8, `2` = 0.8),
  c(`1` = 0.8, `2` = 1.0)
)
pi_vals = list(
  pi = function(t) {
    z <- 1 - 0.25
    tibble(`1` = z, `2` = 1 - z)
  },
  pi = function(t) {
    z <- 1 - 0.5
    tibble(`1` = z, `2` = 1 - z)
  },
  pi = function(t) {
    z <- 1 - 0.75
    tibble(`1` = z, `2` = 1 - z)
  }
)
mu_vals = list(
  `E[X|T,C]` = function(t, c) {
    case_when(c == "1" ~ -1, c == "2" ~ 3.4,
              TRUE ~ NaN)
  },
  `E[X|T,C]` = function(t, c) {
    case_when(c == "1" ~ -1, c == "2" ~ 3.6,
              TRUE ~ NaN)
  },
  `E[X|T,C]` = function(t, c) {
    case_when(c == "1" ~ -1, c == "2" ~ 3.8,
              TRUE ~ NaN)
  },
  `E[X|T,C]` = function(t, c) {
    case_when(c == "1" ~ -1, c == "2" ~ 4.0,
              TRUE ~ NaN)
  },
  `E[X|T,C]` = function(t, c) {
    case_when(c == "1" ~ -1, c == "2" ~ 4.2,
              TRUE ~ NaN)
  },
  `E[X|T,C]` = function(t, c) {
    case_when(c == "1" ~ -1, c == "2" ~ 4.4,
              TRUE ~ NaN)
  },
  `E[X|T,C]` = function(t, c) {
    case_when(c == "1" ~ -1, c == "2" ~ 4.6,
              TRUE ~ NaN)
  },
  `E[X|T,C]` = function(t, c) {
    case_when(c == "1" ~ -1, c == "2" ~ 4.8,
              TRUE ~ NaN)
  },
  `E[X|T,C]` = function(t, c) {
    case_when(c == "1" ~ -1, c == "2" ~ 5.0,
              TRUE ~ NaN)
  }
)

setup_1 = tidyr::expand_grid(mu_vals, pi_vals, sd_vals) %>% mutate(id = row_number()) %>% select(id, everything())



batch_size = 10
run_size = 100

row = (args / run_size) %>% ceiling
arg_within_run = args - (run_size * (row - 1))

iteration_set <- ((batch_size * arg_within_run) - (batch_size - 1)):(batch_size * arg_within_run)






run_name = paste0("setup_1_row_", row, "_12102023_batch")
file_name = paste0(run_name, "_", arg_within_run)
path = paste0(file_name, ".Rdata")


covariate_effect_vector <- c(0)
covariate_list <-  NULL
covariate_names <- NULL
n=300
ncomp = 2

pi = function(t) {
  z <- 0.07 + 0.01 * t - 0.00045 * t^2
  #z <- (1+ exp(-m))^-1 #if exp(m) gets large, it won't add the 1 so we write like this
  tibble("1" = 1 - z, "2" = z)
}

`E[X|T,C]` = function(t, c)
{
  case_when(
    c == "1" ~ -6.0 + 0.2 * t,
    c == "2" ~ 8,
    TRUE ~ NaN
  )
}


t_dist = function(n){runif(n, min = 0, max = 16)}
attr(t_dist, "min") = 0
attr(t_dist, "max") = 16

sd_vector = c("1" = 2, "2" = 1) #0.5, 0.75, 1, 1.25

low_con = -2
high_con = 4


scale = "log"

mu_formula = yi ~ s(t)
pi_formula = c == "2" ~ s(t)
max_it = 3000
ncomp = 2
tol_ll = 1e-6
maxiter_survreg = 30
pause_on_likelihood_drop = TRUE
pi_link = "logit"
verbose = 2
allow_safety = TRUE
cutoff = 0.8
fms_only = FALSE
initial_weighting = 1
keep_true_values = TRUE
conc_limits_table = NULL
max_cens_tolerance = 0.8
