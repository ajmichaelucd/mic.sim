
#Load in the packages I need
library(tidyverse)
library(smoothSurv) #don't use yet but I will once I add semiparametric AFTs
library(mixtools)
library(survival)
library(janitor)
load_all()
#library(ImportExport) #don't use yet, previously I'd been using this to export results to excel files at the end of the run
#library(rlist)
#library(data.table)

index <- "test12722_A"

Sys.time()



##Change these parameters-------------------------------------------------------------------------------------------------------------
#Eventually I want this section to draw from a list of parameters instead of modifying them all individually

#Parameters pertaining to sample size:
norm_mean_sample <- 100 #if doing a normal dist of sample size per year, mean of that distribution
norm_sd_sample <- 20 #if doing a normal dist of sample sizes per year, sd of that distribution
unif_min <- 20 #if doing a uniform dist of sample sizes per year, min of that distribution
unif_max <- 100 #if doing a uniform dist of sample sizes per year, max of that distribution
sample_size_dist <- "normal" #whether you're doing a normal or uniform dist of sample size per year

#General parameters of the simulation
nyears <- 5 #number of years per simulation (numbering will go 0 to n-1 instead of 1 to n)
n_simulations <- 1000 #number of iterations of the simulation to run

#Abx-specific breakpoint (talk to Sharif about whether this is S/I or I/R, looks like we might do I/R)
MIC_breakpoint <- 4 #this value is on the original MIC scale, later in the code i'll take the log2 of it and apply that to dichotomize values

#Distribution of MICs at start of simulation (gaussian mixture, 2 components)
##NEED TO ADD A WAY TO DO 3 COMPONENTS
low_mic_con <- 0.03125 #if MIC is less than or equal to this value, left censored, this is the lowest tested concentration on the scale
high_mic_con <- 64 #if MIC is greater than this value, right censored, this is the highest tested concentration on the scale
initial_means <- c(-3, 2) #starting means of the 2 components of the gaussian mixture for the base MIC distribution in year 0
initial_lower_lambda <- 0.7 #initial value of lambda for the 1st component
initial_upper_lambda <- 0.3 #initial value of lambda for the 2nd component
std_dev_vec <-  c(1,1) #vector of standard deviations of each component (fixed over time)

#Changes in distribution over time, add the trends here
#Trends in component means
slp <-  c(1, 1) #linear slope for changes to mean per year
slp_qd <- c(0,0) #quadractic slope for changes to mean per year
#Trends in component weights
slp_lambda <- c(0, 0) #slope for linear changes in lambda (component weights in the gaussian mixture)

#List of the covariates to include in the simulation
#categorical covariates can have any number of levels, just list the probability for each
#numeric covariates can be either normal or uniform at the moment
#for normal: next two numbers are mean and sd
#for uniform: next two numbers are min and max
covariate_list <- list(
  c("numeric", "normal", 40, 4),
  c("categorical", 0.3, 0.4, 0.3),
  c("categorical", 0.5, 0.2, 0.3),
  c("categorical", 0.2, 0.2, 0.2, 0.2, 0.2),
  c("numeric", "uniform", 1, 10)
)

#What types of AFT error distributions we'll use
type_list = c("loglogistic", "lognormal", "weibull", "gaussian", "exponential", "logistic") #list of types of AFT error distributions to use





#Setup for drawing values ---------------------------------------------------------------------------------
#DON'T EDIT THESE HERE (they're just reformatting the info above for use in commands below)
#these calculate forward the parameters of the gaussian mixture distribution of MICs for each year, using base values and slopes
years <- 0:(nyears - 1)
low_log_con <- log2(low_mic_con)
high_log_con <- log2(high_mic_con)
lower_means <-  initial_means[1] + slp[1] * (years) + slp_qd[1] * (years^2)
upper_means <-  initial_means[2] + slp[2] * (years) + slp_qd[2] * (years^2)
lower_lambda <-  initial_lower_lambda + slp_lambda[1] * years
upper_lambda <-  initial_upper_lambda + slp_lambda[2] * years
year <- years


#this is just to create indices indicating which run all the info came from
sim_id_vec <- seq(n_simulations)

#file name for exported excel file of run results
file_name <- paste(paste("run_results_", index, sep = ""), ".xlsx", sep = "")


#Table Coefficients for each gaussian mixture by year
coef_data <- tibble(lower_means, lsd = rep(std_dev_vec[1], nyears), lower_lambda, upper_means, usd = rep(std_dev_vec[2], nyears), upper_lambda)

#Create cutpoint on log2 scale from MIC breakpoint specified above
cutpoint <- log2(MIC_breakpoint) #okay so cutpoint is on the log2 scale, i think the input should be on the MIC scale so cutpoint will be a function of "MIC_breakpoint"

#Variable gathering for later ---------------
#Take vectors and turn each into an individual variables
initial_lower_mean <- initial_means[1]
initial_upper_mean <- initial_means[2]
initial_lower_slp <- slp[1]
initial_upper_slp <- slp[2]
initial_lower_slp_qd <- slp_qd[1]
initial_upper_slp_qd <- slp_qd[2]
initial_lower_slp_lambda <- slp_lambda[1]
initial_upper_slp_lambda <- slp_lambda[2]
initial_lower_std_dev <- std_dev_vec[1]
initial_upper_std_dev <- std_dev_vec[2]

#Gather all the variables into a single tibble for exporting alongside the results later
presets <- tibble(
  index,
  norm_mean_sample,
  norm_sd_sample,
  unif_min,
  unif_max,
  sample_size_dist,
  nyears,
  n_simulations,
  MIC_breakpoint,
  low_mic_con,
  high_mic_con,
  initial_lower_mean,
  initial_upper_mean,
  initial_lower_slp,
  initial_upper_slp,
  initial_lower_slp_qd,
  initial_upper_slp_qd,
  initial_lower_slp_lambda,
  initial_upper_slp_lambda,
  initial_lower_std_dev,
  initial_upper_std_dev,
  initial_lower_lambda,
  initial_upper_lambda
)


#Function that draws a single observation---------------------------------------------------------------------------------------------------



##Function to draw values for each year-----------
#I'll include this in a later function that will also add covariates and column names


#Functions to draw covariates--------------------------------------------------------------------------------------------------------------



##Create observed data set and covariates-------------------------------------------------------------------------------------------------
##Wraps together the simulation of observations over multiple years, drawing of covariates, and assembling of those two tibbles into one


##Application of drawing a dataset----------------------------------------------------------------------------------------------------
simulated_data <- sim_data_and_add_covariates(covariate_list = covariate_list)


#####NEED A FUNCTION HERE TO ADD COVARIATE EFFECTS#######
#####NEED A FUNCTION HERE TO ADD COVARIATE EFFECTS#######
#####NEED A FUNCTION HERE TO ADD COVARIATE EFFECTS#######
#####NEED A FUNCTION HERE TO ADD COVARIATE EFFECTS#######
#####NEED A FUNCTION HERE TO ADD COVARIATE EFFECTS#######
#####NEED A FUNCTION HERE TO ADD COVARIATE EFFECTS#######











##Fit PARAMETRIC AFT--------------------------------------------------------------------------------------------------------------
##created interval censoring

censor_values <- function(df){
  df %>%
    mutate(right_bound_1 = ceiling(df$observed_value),
           left_bound_1 = ceiling(df$observed_value) - 1) %>%
    mutate(left_bound_cens_1 = ifelse(right_bound_1 <= low_log_con, -Inf, ifelse(left_bound_1 > high_log_con, high_log_con,
                                                                                 left_bound_1)),
           right_bound_cens_1 = ifelse(right_bound_1 > high_log_con, Inf, ifelse(right_bound_1 <= low_log_con, low_log_con,
                                                                                 right_bound_1))) %>%
    mutate(indicator_1 = ifelse(left_bound_cens_1 == -Inf, 2,
                                ifelse(right_bound_cens_1 == Inf, 0, 3))) %>%
    mutate(left_bound_cens_1_exp = exp(left_bound_cens_1),
           right_bound_cens_1_exp = exp(right_bound_cens_1)) %>%
    mutate(dichot = ifelse(observed_value > cutpoint, "R", "S")) %>%
    mutate(year_sq = year^2)
}





#function that fits quadratic aft models and makes a tibble of the results
##NEED TO EDIT THE FORMULA PASTING IF I'M GOING TO HAVE NONLINEAR TRENDS IN YEARS
fit_aft <- function(df1, type){

  if(type %in% c("loglogistic", "weibull", "lognormal", "exponential")){
    outcome <- "surv_object1"
    #variables <-
    variables  <- c("year", paste("covariate_", 1:length(covariate_list), sep = ""))
    f <- as.formula(
      paste(outcome,
            paste(variables, collapse = " + "),
            sep = " ~ "))
    df <- censor_values(df1)
    surv_object1 <- Surv(
      time = ifelse(df$left_bound_cens_1_exp == 0, 0.000000000000000000000000001, df$left_bound_cens_1_exp),
      time2 = df$right_bound_cens_1_exp,
      type = "interval2")


    summary(survreg(print(f), data = df, dist = type)) #THIS NEEDS TO VARY FOR COVARIATES

  }
  else if(type %in% c("logistic", "gaussian")){
    outcome <- "surv_object1"
    variables  <- c("year", paste("covariate_", 1:length(covariate_list), sep = ""))
    f <- as.formula(
      paste(outcome,
            paste(variables, collapse = " + "),
            sep = " ~ "))

    df <- censor_values(df1)
    surv_object1 <- Surv(
      time = df$left_bound_cens_1,
      time2 = df$right_bound_cens_1,
      type = "interval2")


    summary(survreg(print(f), data = df, dist = type)) #THIS NEEDS TO VARY FOR COVARIATES

  }
  else {
    warning("input error, failed to select from weibull, loglogistic, lognormal, exponential, logistic, gaussian")
  }
}


fit_aft(simulated_data$observed_value, simulated_data, "weibull", 4)

##Actually run the aft_fit on dataset---------------------------------------
purrr::map(type_list, ~fit_aft(simulated_data$observed_value, simulated_data, .x, 4))



#To Add:
#Interval Regression
#Semi Parametric AFT
#Logistic Regression

##Way to collect the output of all these models and store that output in a file
#Should I be storing the actual data sets as well? Are these needed for any error metrics we may be interested in?




