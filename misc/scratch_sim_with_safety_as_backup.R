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

#command line arguments------------
args <- as.numeric(commandArgs(trailingOnly = TRUE))

#parameters-------
batch_size <- 10
iteration_set <- ((batch_size * args) - (batch_size - 1)):(batch_size * args) #batch size: 10, so set the subtracted term to be "batch size - 1"

#this set of runs will vary the mean of the upper component and push it closer to the highest tested concentration (2^2)

run_name <- "safety_test_1_03072023"
covariate_effect_vector <- c(0) #0 at start is intercept, then add in the desired coefficients for the covariates
covariate_list <-  NULL
covariate_names <- NULL
n=300
ncomp = 2
pi1 = function(t) {z <- 0.4 #changed to 0.5
c("1" = z, "2" = 1- z)}

`E[X|T,C]` = function(t, c)
{
  case_when(
    c == "1" ~ -1.0 + 0.0 * t,
    c == "2" ~ 3.0 + 0.0 * t,
    TRUE ~ NaN
  )
}

t_dist1 = function(n){runif(n, min = 0, max = 10)}

sd_vector = c("1" = 0.6, "2" = 0.6) #0.5, 0.75, 1, 1.25

low_con = 2^-3
high_con = 2^1 #errored out when this was 2^3
#RUN 1 : 2
#RUN 2: 3
#RUN 3: 4

scale = "log"

formula = Surv(time = left_bound,
               time2 = right_bound,
               type = "interval2") ~ 0 + c + strata(c) + t:c
max_it = 3000
ncomp = 2
tol_ll = 1e-6
maxiter_survreg = 30

#poss_full_sim_in_1_function <- purrr::possibly(.f = full_sim_in_1_function, otherwise = "Error")
#modded_poss_full_sim_in_1_function <- purrr::quietly(full_sim_in_1_function)
#modded_poss_full_sim_in_1_function <- purrr::quietly(poss_full_sim_in_1_function)


#run--------
results <- purrr::map(
  iteration_set,
  ~ full_sim_run_both(
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
    tol_ll = tol_ll,
    verbose = 3,
    maxiter_survreg = maxiter_survreg,
    allow_safety = TRUE
  ))



##add a save here
#results <- purrr::map(full_results, ~add_failure_attr(.x))
#results with new fit_model already capture times survreg ran out of iterations

file_name <- paste(run_name, args, sep = "_")
path <- paste0(file_name, ".Rdata")

save(results, file = path)










