##Matrix Form Approach

##Covariate inputs--------------------
covariate_effect_vector <- c(0, #0 at start is intercept, then add in the desired coefficients for the covariates
                             0.01,
                             c(0.05, 0.08),
                             c(0.2, 0.3),
                             c(0.02, 0.04, 0.06, 0.08),
                             0.05
)

#a is the baseline level for all categorical covariates so, add terms for the coefficients for other levels of the variable that reflect the difference between that level and a

## y = B0 + B1X1 + B2X2b + B3X2c + B4X3b + B5X3c + B6X4b + B7X4c + B8X4d + B9X4e + B10X5 + \epsilon

covariate_list <- list(
  c("numeric", "normal", 40, 4),
  c("categorical", 0.3, 0.4, 0.3),
  c("categorical", 0.5, 0.2, 0.3),
  c("categorical", 0.2, 0.2, 0.2, 0.2, 0.2),
  c("numeric", "uniform", 1, 10)
)

#Initial Distribution Inputs
mean_1_intercept
sd_1
mean_2_intercept
sd_2
pi_1_intercept

#Trend Inputs
mean_1_trend
mean_2_trend
pi_1_trend

#Sample Size Inputs
sample_size_dist
nyears
norm_mean
norm_sd
unif_min
unif_max



##Operations


year <- create_year()

data.sim <- simulate_mics(year = year, mean_2_trend = 0.2 ,covariate_list = covariate_list, covariate_effect_vector = covariate_effect_vector)


purrr::map(type_list, ~fit_aft(data.sim$observed_value, data.sim, .x, summary = TRUE))

fit_lr(data.sim$observed_value, data.sim, MIC_breakpoint = 2, summary = TRUE)

fit_spaft(data.sim$observed_value, data.sim, summary = TRUE)
