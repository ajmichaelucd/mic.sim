library(tidyverse)
library(smoothSurv)
library(mixtools)
library(survival)
library(janitor)
library(ImportExport)
library(rlist)
library(data.table)


#This is the 1st approach I tried, which consists of making a table of the gaussian mixture parameters (component means, sd's, and weights) with...
# an entry for each year in the simulation, then using that table to draaw the appropriate number of observations from each year


index <- "test040622_A" #Add index number for a specific run of the simulation. NOTE: this should later pull from a table of run parameters

cat(Sys.date())


#Below are the edit-able parameters for the simulation, eventually these should all be pulled from a table of run parameters and assigned for the simulation
##Change these parameters-------------------------------------------------------------------------------------------------------------
norm_mean_sample <- 100 #if doing a normal dist of sample size per year, mean of that distribution
norm_sd_sample <- 20 #if doing a normal dist of sample sizes per year, sd of that distribution
unif_min <- 20 #if doing a uniform dist of sample sizes per year, min of that distribution
unif_max <- 100 #if doing a uniform dist of sample sizes per year, max of that distribution
sample_size_dist <- "normal" #whether you're doing a normal or uniform dist of sample size per year

nyears <- 5 #number of years per simulation (numbering will go 0 to n-1 instead of 1 to n)

n_simulations <- 1000 #number of iterations of the simulation to run

MIC_breakpoint <- 4 #this value is on the original MIC scale, later in the code i'll take the log2 of it and apply that to dichotomize values

low_mic_con <- 0.03125 #if MIC is less than or equal to this value, left censored, this is the lowest tested concentration on the scale
high_mic_con <- 64 #if MIC is greater than this value, right censored, this is the highest tested concentration on the scale
initial_means <- c(-3, 2) #starting means of the 2 components of the gaussian mixture for the base MIC distribution in year 0
slp <-  c(0, 0) #linear slope for changes to mean per year
slp_qd <- c(0,0) #quadractic slope for changes to mean per year
slp_lambda <- c(.03, -.03) #slope for linear changes in lambda (component weights in the gaussian mixture)
initial_lower_lambda <- 0.7 #initial value of lambda for the 1st component
initial_upper_lambda <- 0.3 #initial value of lambda for the 2nd component
std_dev_vec <-  c(1,1) #vector of standard deviations of each component (fixed over time)

type_list = c("loglogistic", "lognormal", "weibull", "gaussian", "exponential", "logistic") #list of types of AFT error distributions to use

#Setup for drawing values ---------------------------------------------------------------------------------
#stop editing below this line

#NOTE: these are just some changes I make for the variables above to be used in the simulations below
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



#Draw Values for simulations---------------------------------------------------------------------------------------------------------
#function that takes a vector of years, a tibble of coefficients, a type of distribution (uniform or normal) to draw sample sizes from, and parameters of that distribution (either min and max for uniform or mean and sd for normal)
sim_data <- function(id_arg, year, parameters, type, unif_min, unif_max, norm_mean, norm_sd){
  if(type == "uniform"){
    internal_data <- tibble(year, parameters) #combine parameter table with vector of years (essentially adds a year column to parameter tibble)
    
    internal_data %>% 
      rowwise() %>%
      mutate(nperyear = round(runif(1, unif_min, unif_max))) %>% #draw number of observations for each year (this will be used to tell the next step how many values to draw from a distribution with that year's parameters)
      mutate(values = list(rnormmix(nperyear, lambda = c(lower_lambda, upper_lambda), 
                                    mu = c(lower_means, upper_means),
                                    sigma = c(lsd, usd)))) %>% #draw nperyear values for each row of the parameter tibble (so it mashes all the values for a year into a single row of the tibble corresponding to that year)
      select(year, values) %>% #remove all the parameters of the distribution since we don't need them any more
      unnest_longer(values)%>% 
      mutate(id_arg) %>% #extract all the values that were nested in each row (representing year), now we have a row for each value and its corresponding year
      mutate(id_arg)} 
  
  else if(type == "normal"){
    internal_data <- tibble(year, parameters)
    
    internal_data %>% 
      rowwise() %>%
      mutate(nperyear = abs(round(rnorm(1, norm_mean, norm_sd)))) %>%
      mutate(values = list(rnormmix(nperyear, lambda = c(lower_lambda, upper_lambda), 
                                    mu = c(lower_means, upper_means),
                                    sigma = c(lsd, usd)))) %>% 
      select(year, values) %>% 
      unnest_longer(values) %>% 
      mutate(id_arg)}
  else{print("Select from normal or uniform")}
}
#wrap this into a function and replicate 1000 times, then use purr to run aft's on each faster


#Produces the actual set of raw values for all the simulation iterations
#do n_simulations number of iterations to get that many data sets
drawn_values <- map(sim_id_vec, ~sim_data(.x, years, coef_data, sample_size_dist, norm_mean = norm_mean_sample, norm_sd = norm_sd_sample), simplify = FALSE)


#Modify simulated data for LR and AFTs----------------------------------------------------------------------------------------

#function to add censoring, dichotomize, and restore data to MIC scale before running AFTs
censor_values <- function(df){
  df %>% 
    mutate(right_bound_1 = ceiling(df$values),
           left_bound_1 = ceiling(df$values) - 1) %>% 
    mutate(left_bound_cens_1 = ifelse(right_bound_1 <= low_log_con, -Inf, ifelse(left_bound_1 > high_log_con, high_log_con,
                                                                                 left_bound_1)),
           right_bound_cens_1 = ifelse(right_bound_1 > high_log_con, Inf, ifelse(right_bound_1 <= low_log_con, low_log_con,
                                                                                 right_bound_1))) %>%  
    mutate(indicator_1 = ifelse(left_bound_cens_1 == -Inf, 2, 
                                ifelse(right_bound_cens_1 == Inf, 0, 3))) %>% 
    mutate(left_bound_cens_1_exp = exp(left_bound_cens_1),
           right_bound_cens_1_exp = exp(right_bound_cens_1)) %>% 
    mutate(dichot = ifelse(values > cutpoint, "R", "S")) %>% 
    mutate(year_sq = year^2)
}



#Censor, dichotomize, and put the data on MIC scale
#so one set of maps that apply the function to create interval censoring
censored_df <- map(drawn_values, censor_values)


#Fit PARAMETRIC AFT--------------------------------------------------------------------------------------------------------------

#function that fits quadratic aft models and makes a tibble of the results
fit_and_extract_aft <- function(df, type){
  if(type %in% c("loglogistic", "weibull", "lognormal", "exponential")){
    surv_object1 <- Surv(
      time = ifelse(df$left_bound_cens_1_exp == 0, 0.000000000000000000000000001, df$left_bound_cens_1_exp), 
      time2 = df$right_bound_cens_1_exp,
      type = "interval2")
    
    
    aft_model <- survreg(surv_object1 ~ year + year_sq, data = df, dist = type)
    
    
    tibble(
      distribution = aft_model$dist,
      intercept_coef = aft_model$coefficients [1], #intercept, year coefficients
      year_coef = aft_model$coefficients [2], #intercept, year coefficients
      year_sq_coef = aft_model$coefficients [3], #intercept, year coefficients
      log_scale = log(aft_model$scale) [1], #log scale (so it is on the same scale as the std error)
      std_error_int = sqrt(diag(aft_model$var)) [1], #standard errors for intercept, year, and log scale respectively
      std_error_year = sqrt(diag(aft_model$var)) [2], #standard errors for intercept, year, and log scale respectively
      std_error_year_sq = sqrt(diag(aft_model$var)) [3], #standard errors for intercept, year, and log scale respectively
      std_error_lgscl = sqrt(diag(aft_model$var)) [4], #standard errors for intercept, year, and log scale respectively
      ll_int = aft_model$loglik [1], #log-likelihood (intercept only, then full model)
      ll_full = aft_model$loglik [2] #log-likelihood (intercept only, then full model)
    ) %>% 
      mutate(id_arg = ifelse(length(unique(df$id_arg)) == 1, unique(df$id_arg), warning("data input error")))
  }
  else if(type %in% c("logistic", "gaussian")){
    surv_object1 <- Surv(
      time = df$left_bound_cens_1, 
      time2 = df$right_bound_cens_1,
      type = "interval2")
    
    
    aft_model <- survreg(surv_object1 ~ year + year_sq, data = df, dist = type)
    
    
    tibble(
      distribution = aft_model$dist,
      intercept_coef = aft_model$coefficients [1], #intercept, year coefficients
      year_coef = aft_model$coefficients [2], #intercept, year coefficients
      year_sq_coef = aft_model$coefficients [3], #intercept, year coefficients
      log_scale = log(aft_model$scale) [1], #log scale (so it is on the same scale as the std error)
      std_error_int = sqrt(diag(aft_model$var)) [1], #standard errors for intercept, year, and log scale respectively
      std_error_year = sqrt(diag(aft_model$var)) [2], #standard errors for intercept, year, and log scale respectively
      std_error_year_sq = sqrt(diag(aft_model$var)) [3], #standard errors for intercept, year, and log scale respectively
      std_error_lgscl = sqrt(diag(aft_model$var)) [4], #standard errors for intercept, year, and log scale respectively
      ll_int = aft_model$loglik [1], #log-likelihood (intercept only, then full model)
      ll_full = aft_model$loglik [2] #log-likelihood (intercept only, then full model)
    ) %>% 
      mutate(id_arg = ifelse(length(unique(df$id_arg)) == 1, unique(df$id_arg), warning("data input error")))
  }
  else {
    warning("input error, select from weibull, loglogistic, lognormal, exponential, logistic, gaussian")
  }
}


#map_df(censored_df, ~fit_and_extract_aft(.x, "loglogistic"))
#so this would run a loglogistic on each data set, but now I want more than 1 model type 

#new function that runs the fit and extract aft on each iteration's data set. The purpose of this is to set up a new map command that will repeat this for each type of aft to fit  
aft_multitype <- function(data, type){  
  map_df(data, ~fit_and_extract_aft(.x, type))
}  

aft_results <- map(type_list, ~aft_multitype(censored_df, .x))  






#Fit LOGISTIC REGRESSION--------------------------------------------------------------------------------------------------------

#function that fits LR and extracts model parameters
logreg_fit <- function(df){
  df$dichot <- factor(df$dichot, levels = c("S", "R"))
  lr_model<- glm(dichot ~ year + year_sq, data = df, family = "binomial")
  
  tibble(
    int_coef = coef(summary(lr_model))[1, 1],
    int_se = coef(summary(lr_model))[1, 2],
    int_z = coef(summary(lr_model))[1, 3],
    int_p = coef(summary(lr_model))[1, 4],
    year_coef = coef(summary(lr_model))[2, 1],
    year_se = coef(summary(lr_model))[2, 2],
    year_z = coef(summary(lr_model))[2, 3],
    year_p = coef(summary(lr_model))[2, 4],
    year_sq_coef = coef(summary(lr_model))[3, 1],
    year_sq_se = coef(summary(lr_model))[3, 2],
    year_sq_z = coef(summary(lr_model))[3, 3],
    year_sq_p = coef(summary(lr_model))[3, 4],
    deviance = lr_model$deviance,
    null_deviance = lr_model$null.deviance,
    aic = lr_model$aic
  ) %>% 
    mutate(id_arg = ifelse(length(unique(df$id_arg)) == 1, unique(df$id_arg), warning("data input error")))
}


#apply that lr fit function to each iteration and make results into a single tibble
lr_results <- map_df(censored_df, ~logreg_fit(.x))


#Fit SEMIPARAMETRIC AFT--------------------------------------------------------------------------------------------------------
#then one that fits aft models
fit_and_extract_spaft <- function(df){
  
  surv_object1 <- Surv(
    time = ifelse(df$left_bound_cens_1_exp == 0, 0.000000000000000000000000001, df$left_bound_cens_1_exp), 
    time2 = df$right_bound_cens_1_exp,
    type = "interval2")
  
  
  spaft_model <- smoothSurvReg(surv_object1 ~ year + year_sq, data = df)
  #summary(aft_parametric_weibull)
  
  
  
  #regres holds coef, std error (2 types of se)
  
  tibble(
    int_coef = spaft_model$regres[1,1],
    int_se1 = spaft_model$regres[1,2],
    int_se2 = spaft_model$regres[1,3],
    year_coef = spaft_model$regres[2,1],
    year_se1 = spaft_model$regres[2,2],
    year_se2 = spaft_model$regres[2,3],
    year_sq_coef = spaft_model$regres[3,1],
    year_sq_se1 = spaft_model$regres[3,2],
    year_sq_se2 = spaft_model$regres[3,3],
    logscale_value = spaft_model$regres[4,1],
    logscale_se1 = spaft_model$regres[4,2],
    logscale_se2 = spaft_model$regres[4,3],
    ll = spaft_model$loglik[1],
    penalty = spaft_model$loglik[2],
    penalized_ll = spaft_model$loglik[3],
    aic = spaft_model$aic,
    smooth_lambda = spaft_model$degree.smooth[1],
    log_smooth_lambda = spaft_model$degree.smooth[2],
    smooth_df = spaft_model$degree.smooth[3],
    number_smooth_param = spaft_model$degree.smooth[4],
    number_mean_param = spaft_model$degree.smooth[5],
    number_scale_param = spaft_model$degree.smooth[6],
    number_spline_param = spaft_model$degree.smooth[7]
  ) %>% 
    mutate(id_arg = ifelse(length(unique(df$id_arg)) == 1, unique(df$id_arg), warning("data input error")))
}


#spaft_results <- map_df(censored_df, ~fit_and_extract_spaft(.x))
spaft_results <- NA

#Break down individual data sets---------------------------------------------------------------------------------------------
drawn_values_summaries <- function(df, a){
  
  if(a == "centers"){
    df %>% 
      group_by(year) %>% 
      summarise(mean = mean(values),
                median = median(values)
      ) %>% 
      mutate(id_arg = ifelse(length(unique(df$id_arg)) == 1, unique(df$id_arg), warning("data input error")))
  } 
  else if (a == "censoring"){
    df %>% 
      group_by(year, indicator_1) %>% 
      summarise(count = n(),  .groups = "keep") %>% 
      pivot_wider(names_from = indicator_1, values_from = count) %>% 
      mutate(id_arg = ifelse(length(unique(df$id_arg)) == 1, unique(df$id_arg), warning("data input error")))
  }
  
  ##can add more operations here!!
  
  else{print("choose centers or censoring")}
}

data_centers1 <- map(censored_df, ~drawn_values_summaries(.x, "centers")) 
#can push to 1 tibble with rbindlist from data.table package
data_centers <- tibble(rbindlist(data_centers1))


data_censoring1 <- map(censored_df, ~drawn_values_summaries(.x, "censoring"))
data_censoring <- tibble(rbindlist(data_censoring1, fill = TRUE))

##Data to export:



name_add_index <- function(names_vec){paste(names_vec,index, sep = "")}
#aft_names <- map( c("loglogistic_aft_", "lognormal_aft_", "weibull_aft_", "gaussian_aft_", "exponential_aft_", "logistic_aft_"), ~name_add_index(.x))

#names(aft_results) <- aft_names

aft_results_merged <- tibble(rbindlist(aft_results))
censored_df_merged <- tibble(rbindlist(censored_df))


#assign(paste("data_centers_", index, sep = ""), data_centers),
#assign(paste("data_censoring_", index, sep = ""), data_censoring)
#assign(paste("aft_results_", index, sep = ""), aft_results_merged)
#assign(paste("lr_results_", index, sep = ""), lr_results)
#assign(paste("spaft_results_", index, sep = ""), spaft_results)


run_results <- list(data_centers, data_censoring, aft_results_merged, lr_results, spaft_results, coef_data, presets, censored_df_merged)

names(run_results) <- map(c("data_centers_", "data_censoring_", "aft_results_", "lr_results_", "spaft_results_", "coef_data_", "presets_", "censored_df_"), name_add_index)




#assign(paste("run_results_", index, sep = ""), run_results) #model results and some summaries of data
#assign(paste("censored_df_", index, sep = ""), censored_df) #all the data sets

cat(Sys.date())

assign(paste("run_results_df_", index, sep = ""), map(run_results, data.frame)) %>% 
  excel_export( . , file_name, table_names = names(run_results))


