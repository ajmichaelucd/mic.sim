Sys.setlocale (locale = "en_US.UTF-8")
print(sort(c("10", "1:")))

packages <- c("magrittr","dplyr","tidyr","mic.sim","LearnBayes","survival","gridExtra", "data.table")
inst <- packages %in% installed.packages()
if (length(packages[!inst])>0) install.packages(packages[!inst],dependencies = T)
lapply(packages,require,character.only=TRUE)

library(magrittr)
library(dplyr)
library(tidyr)
#load_all()
library(mic.sim)
#library(ggplot2)
library(LearnBayes)
library(survival)
#library(biglm)
library(gridExtra)
library(data.table)
library(purrr)
library(stringr)
library(gam)

#command line arguments------------
args <- as.numeric(commandArgs(trailingOnly = TRUE))

#parameters-------
batch_size <- 10
iteration_set <- ((batch_size * args) - (batch_size - 1)):(batch_size * args) #batch size: 10, so set the subtracted term to be "batch size - 1"

#this set of runs will vary the mean of the upper component and push it closer to the highest tested concentration (2^2)

run_name <- "safety_test_cutoff_check_1_04112023"
covariate_effect_vector <- c(0) #0 at start is intercept, then add in the desired coefficients for the covariates
covariate_list <-  NULL
covariate_names <- NULL
n=300
ncomp = 2
#pi_truth = "identity"


pi1 = function(t) {
  m <- 0.5 + 0.1 * t - 0.03 * t^2 + 0.0015 * t^3
  z <- (1+ exp(-m))^-1 #if exp(m) gets large, it won't add the 1 so we write like this
  tibble("1" = 1 - z, "2" = z)
}

#pi1 =   function(t) {m <- 0.2 + 0.001 * t   #logit
#  z <- exp(m) / (1+ exp(m))
#  c("1" = z, "2" = 1 - z)}
#

`E[X|T,C]` = function(t, c)
{
  case_when(
    c == "1" ~ -1.0 - 0.02 * t,
    c == "2" ~ 4.0 + 0.0 * t,
    TRUE ~ NaN
  )
}

t_dist1 = function(n){runif(n, min = 0, max = 15)}
attr(t_dist1, "min") = 0
attr(t_dist1, "max") = 0

sd_vector = c("1" = 0.6, "2" = 0.6) #0.5, 0.75, 1, 1.25

low_con = -3
high_con = 1 #errored out when this was 2^3
#RUN 1 : 2
#RUN 2: 3
#RUN 3: 4

scale = "log"

formula = Surv(time = left_bound,
               time2 = right_bound,
               type = "interval2") ~ pspline(t, df = 0, calc = TRUE)
formula2 = c == "1" ~ s(t)
max_it = 3000
ncomp = 2
tol_ll = 1e-6
maxiter_survreg = 30
pi_function = TRUE
pi_link = "identity"
verbose = 2
allow_safety = TRUE
cutoff = 0.7
fms_only = FALSE
initial_weighting = 1
keep_true_values = TRUE

#poss_full_sim_in_1_function <- purrr::possibly(.f = full_sim_in_1_function, otherwise = "Error")
#modded_poss_full_sim_in_1_function <- purrr::quietly(full_sim_in_1_function)
#modded_poss_full_sim_in_1_function <- purrr::quietly(poss_full_sim_in_1_function)

#run--------
model_results <- purrr::map(
  iteration_set,
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
    formula2 = formula2,
    max_it = max_it,
    ncomp = ncomp,
    tol_ll = tol_ll,
    verbose = verbose,
    maxiter_survreg = maxiter_survreg,
    pi_function = pi_function,
    pi_link = pi_link,
    allow_safety = allow_safety,
    cutoff = cutoff,
    fms_only = fms_only,
    initial_weighting = initial_weighting,
    keep_true_values = keep_true_values
  ))

results <- list(
  model_results = model_results,
  settings = list( #name all settings, at batch level: create settings and save along with results
    iteration_set = iteration_set,
    n = n,
    t_dist = t_dist1,
    pi = pi1,
    `E[X|T,C]` = `E[X|T,C]`,
    sd_vector = sd_vector,
    covariate_list = covariate_list,
    covariate_effect_vector = covariate_effect_vector,
    covariate_names = covariate_names,
    conc_limits_table = conc_limits_table,
    low_con = low_con,
    high_con = high_con,
    scale = scale,
    formula = formula,
    formula2 = formula2,
    max_it = max_it,
    ncomp = ncomp,
    tol_ll = tol_ll,
    maxiter_survreg = maxiter_survreg,
    pi_link = pi_link,
    verbose = verbose,
    allow_safety = allow_safety,
    cutoff = cutoff,
    fms_only = fms_only,
    initial_weighting = initial_weighting,
    keep_true_values = keep_true_values
  ))

##add a save here
#results <- purrr::map(full_results, ~add_failure_attr(.x))
#results with new fit_model already capture times survreg ran out of iterations

file_name <- paste(run_name, args, sep = "_")
path <- paste0(file_name, ".Rdata")

save(results, file = path)










