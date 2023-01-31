#Load in packages
setwd("~/Desktop/Dissertation Project/Chapter 1/mic.sim")
load_all()
library(dplyr)
library(magrittr)
library(purrr)
library(survival)
library(gridExtra)
library(data.table)
library(readxl)

#Edit these parameters

location <- "~/Desktop/"
number_per_batch <- 10
array_name <- "censor_mean_2_sd_0.8_pi2_0.8_run_16"
date <- "01032023"
covariate_list <-  NULL
covariate_effect_vector <- c(0)
scale = "log"


args = 1


##Stop Edits
    Sys.setlocale (locale = "en_US.UTF-8")
    if(!identical(sort(c("10", "1:")), c("1:", "10"))){
      errorCondition("sort error")
    }

    parameter_log <- read_excel("~/Desktop/Sim_Results/simulation_parameter_log.xlsx",
                                sheet = "Sheet2")

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

    setwd(location)


    #command line arguments------------

    batch_size <- number_per_batch
    #batch size: 10, so set the subtracted term to be "batch size - 1"
    #parameters-------

    #this set of runs will vary the sd of the upper component and push it closer to the highest tested concentration (2^2)

    run_name <- paste(array_name, date, sep = "_")
    covariate_effect_vector <- covariate_effect_vector #0 at start is intercept, then add in the desired coefficients for the covariates
    covariate_list <-  covariate_list
    covariate_names <- NULL
    n=n
    ncomp = 2
    pi1 = function(t) {z <- pi[1] #changed to 0.5
    c("1" = z, "2" = 1- z)}

    `E[X|T,C]` = function(t, c)
    {
      case_when(
        c == "1" ~ intercepts[1] + trends[1] * t,
        c == "2" ~ intercepts[2] + trends[2] * t,
        TRUE ~ NaN
      )
    }

    t_dist1 = function(n){runif(n, min = 0, max = nyears)}

    sd_vector = c("1" = sigma[1], "2" = sigma[2]) #0.5, 0.75, 1, 1.25

    low_con = low_con
    high_con = high_con #errored out when this was 2^3
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

modded_local_full_run_function <- function(args,
                                        batch_size = 10,
                                        run_name,
                                        n,
                                        t_dist,
                                        pi,
                                        `E[X|T,C]`,
                                        sd_vector,
                                        covariate_list = NULL,
                                        covariate_effect_vector = c(0),
                                        covariate_names = NULL,
                                        low_con,
                                        high_con,
                                        scale = "log",
                                        formula,
                                        max_it = 3000,
                                        ncomp = 2,
                                        tol_ll = 1e-6,
                                        verbose = 3){
      iteration_set <- ((batch_size * args) - (batch_size - 1)):(batch_size * args)

      poss_full_sim_in_1_function <- purrr::possibly(.f = full_sim_in_1_function, otherwise = "Error")
      #modded_poss_full_sim_in_1_function <- purrr::quietly(full_sim_in_1_function)
      modded_poss_full_sim_in_1_function <- purrr::quietly(poss_full_sim_in_1_function)

#   attr_modded_poss_full_sim_in_1_function <- function(i, n, t_dist, pi, `E[X|T,C]`, sd_vector, covariate_list, covariate_effect_vector, covariate_names, low_con, high_con, scale, formula, max_it, ncomp, tol_ll, verbose){
#       full_results <- modded_poss_full_sim_in_1_function(
#         i = i,
#         n = n,
#         t_dist = t_dist,
#         pi = pi,
#         `E[X|T,C]` = `E[X|T,C]`,
#         sd_vector = sd_vector,
#         covariate_list = covariate_list,
#         covariate_effect_vector = covariate_effect_vector,
#         covariate_names = NULL,
#         low_con = low_con,
#         high_con = high_con,
#         scale = scale,
#         formula = formula,
#         max_it = max_it,
#         ncomp = ncomp,
#         tol_ll = tol_ll,
#         verbose = verbose
#       )
#
#       results <- full_results$result
#       attr(results, "survreg_failure") <- any(stringr::str_detect(full_results$warnings, "Ran out of iterations and did not converge"))
#       return(results)
# }

      #run--------
      # results <- purrr::map(
      #   iteration_set,
      #   ~ attr_modded_poss_full_sim_in_1_function(
      #     .x,
      #     n = n,
      #     t_dist = t_dist,
      #     pi = pi,
      #     `E[X|T,C]` = `E[X|T,C]`,
      #     sd_vector = sd_vector,
      #     covariate_list = covariate_list,
      #     covariate_effect_vector = covariate_effect_vector,
      #     covariate_names = NULL,
      #     low_con = low_con,
      #     high_con = high_con,
      #     scale = scale,
      #     formula = formula,
      #     max_it = max_it,
      #     ncomp = ncomp,
      #     tol_ll = tol_ll,
      #     verbose = 3
      #   ))
      #
  full_results <- purrr::map(
    iteration_set,
    ~ modded_poss_full_sim_in_1_function(
      .x,
      n = n,
      t_dist = t_dist,
      pi = pi,
      `E[X|T,C]` = `E[X|T,C]`,
      sd_vector = sd_vector,
      covariate_list = covariate_list,
      covariate_effect_vector = covariate_effect_vector,
      covariate_names = NULL,
      low_con = low_con,
      high_con = high_con,
      scale = scale,
      formula = formula,
      max_it = max_it,
      ncomp = ncomp,
      tol_ll = tol_ll,
      verbose = 3
    ))

#add_failure_attr <- function(full_results){
#  results <- full_results$result
#  attr(results, "survreg_failure") <- any(stringr::str_detect(full_results$warnings, "Ran out of iterations and did not converge"))
#  results
#}

results <- purrr::map(full_results, ~add_failure_attr(.x))

      ##add a save here
#results <- full_results[[1]]$result
#attr(results, "survreg_failure") <- any(stringr::str_detect(full_results[[1]]$warnings, "Ran out of iterations and did not converge"))
    #if placed here, they just grab one object from the purrr list output, so it doesn't do it for each object

      file_name <- paste(run_name, args, sep = "_")
      path <- paste0(file_name, ".Rdata")

      save(results, file = path)
    }





    for(i in args){modded_local_full_run_function(
      args = i,
      batch_size = batch_size,
      run_name = run_name,
      n = n,
      t_dist = t_dist1,
      pi = pi1,
      `E[X|T,C]` = `E[X|T,C]`,
      sd_vector = sd_vector,
      covariate_list = NULL,
      covariate_effect_vector = c(0),
      covariate_names = NULL,
      low_con = low_con,
      high_con = high_con,
      scale = "log",
      formula = formula,
      max_it = 3000,
      ncomp = 2,
      tol_ll = 1e-6,
      verbose = 3
    )
    }

    setwd(
      "~/Desktop/Dissertation Project/Chapter 1/mic.sim"
    )



 ###scratchwork for changes to post-processing

load("~/Desktop/censor_mean_2_sd_0.8_pi2_0.8_run_16_01032023_1.Rdata")




results































for(i in 1:10){

#print(i)
##print(results[[i]]$warnings)
#
#
#b <- results[[i]]$result
#
##for some reason if I don't use a named object instead of results[[1]]$result
#a <- append(b, list(results[[i]]$warning)) ##need results[[1]]$warning to enter as only one item in list, this doesn't work
#
#if(length(a) > 3){
#
#    print(any(stringr::str_detect(a[[6]], "Ran out of iterations and did not converge"))) }
#

#add options for no warnings [[6]] is zero and errors (no [[6]])


c <- results[[9]]
#c
g <- c$result
#c$warnings

attr(g, "survreg_failure") <- any(stringr::str_detect(c$warnings, "Ran out of iterations and did not converge"))
g
}
#list(c$result, print(any(stringr::str_detect(c$warnings, "Ran out of iterations and did not converge"))))

#length(c)
