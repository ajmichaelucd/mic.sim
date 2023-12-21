Sys.setlocale (locale = "en_US.UTF-8")
print(sort(c("10", "1:")))

library(magrittr)
library(dplyr)
library(tidyr)
library(mic.sim)
library(LearnBayes)
library(survival)
library(gridExtra)
library(data.table)
library(purrr)
library(stringr)
library(mgcv)

#command line arguments------------
args <- as.numeric(commandArgs(trailingOnly = TRUE))

#parameters-------
#we fit 100 models for each of 10 mic seeds,
#and do 10 runs of this per set of initial parameters (so 10 args per set of parameters),
#then we need to expand 1 arg into 10 mic seeds and 100 model start seeds

#so 1 batch script for 12 combos of parameters would need 120 jobs



n_models_per_data_set = 100
n_data_sets_per_run = 10
n_runs_per_setup = 10
n_setups = 12


mic.seed = (((args - 1) * n_data_sets_per_run) + 1): (args* n_data_sets_per_run)
get_model_start_seeds =
  function(mic.seed = .x, n_models_per_data_set) {
    (((mic.seed - 1) * n_models_per_data_set) + 1):(mic.seed * n_models_per_data_set)
  }

random_start_seeds = map(mic.seed, ~get_model_start_seeds(.x, n_models_per_data_set))

sd_vals = list(
  0.1,
  0.2,
  0.3,
  0.4
)

pi_vals = list(
  function(t) {
    z <- 0 + (0.002 * t)
    #z <- (1+ exp(-m))^-1 #if exp(m) gets large, it won't add the 1 so we write like this
    tibble("1" = 1 - z, "2" = z)
  },
  function(t) {
    z <- 0.1 + (0.002 * t)
    #z <- (1+ exp(-m))^-1 #if exp(m) gets large, it won't add the 1 so we write like this
    tibble("1" = 1 - z, "2" = z)
  },
  function(t) {
    z <- 0.2 + (0.002 * t)
    #z <- (1+ exp(-m))^-1 #if exp(m) gets large, it won't add the 1 so we write like this
    tibble("1" = 1 - z, "2" = z)
  }
  )

changing_parameters = tidyr::expand_grid(sd_vals = sd_vals, pi_vals = pi_vals) %>%
  mutate(id = row_number()) %>%
  filter(id == ceiling(args / n_runs_per_setup))


run_name = paste0("setup_1_row_", ceiling(args / n_runs_per_setup), "_12212023_run")
file_name <- paste(run_name, args, sep = "_")
path <- paste0(file_name, ".Rdata")

##set parameters------
n = 300
t_dist = function(n){runif(n, min = 0, max = 16)}
#pi = function(t) {
#  z <- 0.01 + (0.002 * t)
#  #z <- (1+ exp(-m))^-1 #if exp(m) gets large, it won't add the 1 so we write like this
#  tibble("1" = 1 - z, "2" = z)
#}

pi = (changing_parameters$pi_vals)[[1]]
`E[X|T,C]` = function(t, c){
  case_when(
    c == "1" ~ -3.0 + 0.2 * t,
    c == "2" ~ -1 + (15 * sqrt((t ^ 0.7) * 0.02)),
    TRUE ~ NaN
  )
}
ggplot() +
  geom_function(fun = function(t){`E[X|T,C]`(t, c = "1")}) + xlim(0, 16) +
  geom_function(fun = function(t){`E[X|T,C]`(t, c = "2")})
sd_vector = c("1" = 1, "2" = 1)
low_con = -5
high_con = 3
scale = "log"
model = "fm"
mu_formula = Surv(time = left_bound,
                  time2 = right_bound,
                  type = "interval2") ~ pspline(t, df = 0, caic = TRUE)
pi_formula = c == "2" ~ s(t)
max_it = 500
ncomp = 2
tol_ll = 1e-5
pi_link = "logit"
verbose = 3
initial_weighting = 7
model_coefficient_tolerance = 0.0001
maxiter_survreg = 30
sd_initial = (changing_parameters %>% pull(sd_vals))[[1]]
randomize = "all"
censored_side = NULL
extra_row = NULL

##run model------
batch_output =
  map2(mic.seed,
     random_start_seeds,
     ~simulation_1_set(
       .x,
       .y,
       n = n,
       t_dist = t_dist,
       pi = pi,
       `E[X|T,C]` = `E[X|T,C]`,
       sd_vector = sd_vector,
       low_con = low_con,
       high_con = high_con,
       scale = scale,
       model = model,
       mu_formula = mu_formula,
       pi_formula = pi_formula,
       max_it = max_it,
       ncomp = ncomp,
       tol_ll = tol_ll,
       pi_link = pi_link,
       verbose = verbose,
       initial_weighting = initial_weighting,
       model_coefficient_tolerance = model_coefficient_tolerance,
       maxiter_survreg = maxiter_survreg,
       sd_initial = sd_initial,
       randomize = randomize,
       censored_side = censored_side,
       extra_row = extra_row
     )
  )

##save parameters------
batch_parameters = list(
  mic.seed = mic.seed,
  random_seeds_vector = random_seeds_vector,
  n = n,
  t_dist = t_dist,
  pi = pi,
  `E[X|T,C]` = `E[X|T,C]`,
  sd_vector = sd_vector,
  low_con = low_con,
  high_con = high_con,
  scale = scale,
  model = model,
  mu_formula = mu_formula,
  pi_formula = pi_formula,
  max_it = max_it,
  ncomp = ncomp,
  tol_ll = tol_ll,
  pi_link = pi_link,
  verbose = verbose,
  initial_weighting = initial_weighting,
  model_coefficient_tolerance = model_coefficient_tolerance,
  maxiter_survreg = maxiter_survreg,
  sd_initial = sd_initial,
  randomize = randomize,
  censored_side = censored_side,
  extra_row = extra_row
)


##save_output------
results = list(
  batch_output = batch_output,
  batch_parameters = batch_parameters)

save(results, file = path)
