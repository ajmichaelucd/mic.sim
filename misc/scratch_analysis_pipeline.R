library(magrittr)
library(dplyr)
library(tidyr)
library(mic.sim)
library(stringr)
library(data.table)
library(purrr)
library(survival)
library(mgcv)
library(ggplot2)

#grab this info from setup file
run_name = "setup_2"
date = "12212023"
files_folder = ""


n_models_per_data_set = 100
n_data_sets_per_run = 2
n_runs_per_setup = 50
n_setups = 12

args <- as.numeric(commandArgs(trailingOnly = TRUE))

#1:(n_runs_per_setup * n_setups)

#map over setups
#parsed_results = map(1:n_setups, ~analysis_one_setup(setup_number = .x, files_folder, run_name, n_runs_per_setup, date)) %>%
#  save(parsed_results, file = path)

#setup_number = 1

  parsed_results = analysis_one_setup(setup_number = args, files_folder, run_name, n_runs_per_setup, date)
  path = paste0(run_name, "_analysis_set_", args, ".Rdata")
  save(parsed_results, file = path)




