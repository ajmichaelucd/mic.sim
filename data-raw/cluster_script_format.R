library(magrittr)
library(dplyr)
library(tidyr)
#load_all()
library(mic.sim)
#library(ggplot2)
library(LearnBayes)
library(survival)
library(biglm)
library(gridExtra)
library(data.table)

#command line arguments------------
args <- commandArgs(trailingOnly = TRUE)
iteration_set <- ((10 * args) - 9):(10 * args)
#parameters-------
run_name <- "small_trend_run_2_09012022"
iterations <- 1 #100
covariate_effect_vector <- c(0) #0 at start is intercept, then add in the desired coefficients for the covariates
covariate_list <-  NULL
covariate_names <- NULL
n=2000
ncomp = 2
pi1 = function(t) {z <- 0.8
c("1" = z, "2" = 1- z)}

`E[X|T,C]` = function(t, c)
{
  case_when(
    c == "1" ~ 0 + 0.05 * t,
    c == "2" ~ 1.5 + 0 * t,
    TRUE ~ NaN
  )
}

t_dist1 = function(n){runif(n, min = 0, max = 5)}

sd_vector = c("1" = 1, "2" = 0.5)

low_con = 2^-2
high_con = 2^5 #errored out when this was 2^3

scale = "log"

formula = Surv(time = left_bound,
               time2 = right_bound,
               type = "interval2") ~ 0 + c + strata(c) + t:c
max_it = 3000
ncomp = 2
tol_ll = 1e-6


#run--------
results <- purrr::map(
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
    max_it = max_it,
    ncomp = ncomp,
    tol_ll = tol_ll,
    silent = TRUE
  ))



##add a save here



file_name <- paste(run_name, args, sep = "_")
path <- paste0(file_name, ".Rdata")

save(results, file = path)


















