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
n_batches_per_set = 10
n_models_per_set = 100
n_models_per_batch = n_models_per_set / n_batches_per_set
mic.seed = ceiling(args / 10)
random.start.seeds = (1 + (n_models_per_batch * (args - 1))):(10 + (n_models_per_batch * (args - 1)))


#map over random.start.seeds
