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
full_sim_2 <- function(i,
                       n = 150,
                       t_dist = function(n){runif(n, min = 0, max = 5)},
                       pi = function(t) {z <- 0.6 #0.5 + 0.2 * t
                       c("1" = z, "2" = 1 - z)},
                       `E[X|T,C]` = function(t, c)
                       {
                         case_when(
                           c == "1" ~ -2 -0.1*t, #3 + t + 2*t^2 - sqrt(t),
                           c == "2" ~ 2 + 0.2*t,
                           TRUE ~ NaN
                         )
                       },
                       sd_vector = c("1" = 1, "2" = 2),
                       covariate_list = NULL,
                       covariate_effect_vector = c(0),
                       covariate_names = NULL,
                       conc_limits_table = NULL, #conc_limits_table = as_tibble(rbind(c("a", 2^-3, 2^3),
                       #c("b", 2^-4, 2^4)), `.name_repair` = "unique"
                       #) %>% rename("covariate_2" = 1, "low_cons" = 2, "high_cons" = 3),
                       low_con = 2^-4,
                       high_con = 2^4,
                       scale = "log",
                       formula = Surv(time = left_bound,
                                      time2 = right_bound,
                                      type = "interval2") ~ 0 + c + strata(c) + t:c,
                       max_it = 3000,
                       ncomp = 2,
                       tol_ll = 1e-6,
                       #silent = FALSE,
                       maxiter_survreg = 30,
                       verbose = 3,
                       allow_safety = TRUE,

                       ...
){
  set.seed(i)
  if(verbose > 2){
    message("starting run number", i)}
  #verbose = 0: print nothing
  #verbose = 1: print run number
  #verbose = 2: print run number and iteration number
  #verbose = 3: print run number, iteration number, and iteration results
  #verbose = 4: print run number, iteration number, iteration results, and run aft as verbose



  #mem here

  data.sim <- simulate_mics_2( #changed to test
    n = n,
    t_dist = t_dist,
    pi = pi,
    `E[X|T,C]` = `E[X|T,C]`,
    sd_vector = sd_vector,
    covariate_list = covariate_list,
    covariate_effect_vector = covariate_effect_vector,
    conc_limits_table = conc_limits_table,
    low_con = low_con,
    high_con = high_con,
    scale = "log")

  #mem here

  visible_data <- prep_sim_data_for_em(data.sim, left_bound_name = "left_bound", right_bound_name = "right_bound", time = "t", covariate_names, scale = scale)

  #mem here
  poss_fit_model <- purrr::possibly(.f = fit_model, otherwise = "Error")
  single_model_output = fit_model(visible_data = visible_data, formula = formula, max_it = max_it, ncomp = ncomp, tol_ll = tol_ll, verbose = verbose, maxiter_survreg = maxiter_survreg)
  #single_model_output = fit_model(visible_data = visible_data, formula = formula, max_it = max_it, ncomp = ncomp, tol_ll = tol_ll, verbose = verbose, maxiter_survreg = maxiter_survreg, ...)

  #wrap fit model with possibly() or try()

  if(length(single_model_output) == 1 && single_model_output == "Error"){
    fm_fail = "fm_failed"
  } else{
    fm_fail = "fm_worked"
  }

  if(length(single_model_output) == 1 && single_model_output == "Error" & allow_safety == TRUE){
    formula = Surv(time = left_bound,
                   time2 = right_bound,
                   type = "interval2") ~ t
    poss_fit_model_safety <- purrr::possibly(.f = fit_model_safety, otherwise = "Error")
    #single_model_output = fit_model_safety(visible_data = visible_data, formula = formula, max_it = max_it, ncomp = ncomp, tol_ll = tol_ll, verbose = verbose, maxiter_survreg = maxiter_survreg, ...)
    single_model_output = fit_model_safety(visible_data = visible_data, formula = formula, max_it = max_it, ncomp = ncomp, tol_ll = tol_ll, verbose = verbose, maxiter_survreg = maxiter_survreg)
    if(length(single_model_output) == 1 && single_model_output == "Error"){
      fms_fail = "fms_failed"
    } else{
      fms_fail = "fms_worked"
    }


  } else{
    fms_fail = "fms_not_called"
  }

  #evaluate
  #run fit_model_safely if needed

  failure_safety_notes <- c(allow_safety, fm_fail, fms_fail)


  single_model_output <- append(single_model_output, i)
  single_model_output <- append(single_model_output, failure_safety_notes)
  #return

  return(single_model_output)

}

#run--------
results <- purrr::map(
  iteration_set,
  ~ full_sim_2(
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










