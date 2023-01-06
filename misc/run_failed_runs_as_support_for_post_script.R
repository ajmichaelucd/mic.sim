Sys.setlocale (locale = "en_US.UTF-8")
print(sort(c("10", "1:")))

setwd(
  "~/Desktop/Dissertation Project/Chapter 1/mic.sim"
)

load_all()

#packages <- c("magrittr","dplyr","tidyr","mic.sim","LearnBayes","survival","gridExtra", "data.table")
#inst <- packages %in% installed.packages()
#if (length(packages[!inst])>0) install.packages(packages[!inst],dependencies = T)
#lapply(packages,require,character.only=TRUE)

#library(magrittr)
#library(dplyr)
#library(tidyr)
##load_all()
#library(mic.sim)
##library(ggplot2)
#library(LearnBayes)
#library(survival)
##library(biglm)
#library(gridExtra)
#library(data.table)


rerun_incomplete_sets <- function(location, incomplete, number_per_batch, array_name, date, covariate_effect_vector, covariate_list, n, pi, intercepts, trends, sigma, nyears, low_con, high_con){

  Sys.setlocale (locale = "en_US.UTF-8")
  if(sort(c("10", "1:")) != c("1:", "10")){
    errorCondition("sort error")
  }

setwd(location)


#command line arguments------------
args <- incomplete %>% pull(incomplete) %>% as.vector()
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

for(i in args){local_full_run_function(
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


}



