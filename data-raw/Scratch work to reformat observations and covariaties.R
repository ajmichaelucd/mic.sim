


#generate year, then run find_epison with mean and pi as functions of year



find_epsilon <- function(year, sd_1,  sd_2, mean_1_trend, mean_2_trend, mean_1_intercept, mean_2_intercept, pi_1_trend, pi_1_intercept){
  dataset <- tibble(
    year,
    mean_1 = year * mean_1_trend + mean_1_intercept,
    mean_2 = year * mean_2_trend + mean_2_intercept,
    pi_1 = year * pi_1_trend + pi_1_intercept) %>%
    rowwise() %>%
    mutate(
    w = rbinom(1, 1, pi_1), #pi_1 should be a function of year
    u = rnorm(1, mean_1, sd_1), #mean_1 should be a function of year
    v = rnorm(1, mean_2, sd_2), #mean_2 should be a function of year
    epsilon = w * u + (1 - w) * v
  ) %>%
    ungroup()
}

aaa <- find_epsilon(year = year, sd_1 = 1, sd_2 = 1, mean_1_trend = 0, mean_2_trend = 0.5, mean_1_intercept = -1, mean_2_intercept = 2, pi_1_trend = 0, pi_1_intercept = 0.4)






add_covariate <- function(....){ #pass 'n' in to the function where n = nrow from epsilon tibble
  covariate1 <- rnorm(n_total, cov1_mean, cov1_sd) #this all becomes if statements to programmatically create matrix
  covariate2 <- runif(n_total, cov2_min, cov2_max)
  covariate3 <- sample(letters[1:3], n_total, c(0.2, 0.4, 0.6)) #%>%
}

#bind_cols (dplyr)

# covariate1 <- rnorm(20, 3, 1) #this all becomes if statements to programmatically create matrix
# covariate2 <- runif(20, 0, 10)
# covariate3 <- sample(letters[1:3], 20, c(0.2, 0.4, 0.6), replace = TRUE) #%>%
##include these in the tibble






add_covariate <- function(covariate_list, year){
  year <- as_tibble(year)
 covariates <- as_tibble(purrr::map(covariate_list, ~draw_covariates(year, .x)),  .name_repair = "unique" )
  names(covariates) <- create_cov_names(covariates)
tibble(covariates)
}

covariate_list <- list(
  c("numeric", "normal", 40, 4),
  c("categorical", 0.3, 0.4, 0.3),
  c("categorical", 0.5, 0.2, 0.3),
  c("categorical", 0.2, 0.2, 0.2, 0.2, 0.2),
  c("numeric", "uniform", 1, 10)
)


bbb <- add_covariate(covariate_list = covariate_list, year = year)


model_matrix <- function(data_tibble, covariate_effect_vector){
#f <- as.formula(paste("~", paste(names(bbb), collapse = " + "), sep = ""))
f <- data_tibble %>%
    select(starts_with("covariate"))

#model.matrix(f) %*% covariate_effect_vector
  model.matrix(data = f, ~.) %*% covariate_effect_vector
  }


#generate with as.formula()
 #then add in epsilon
##instead of as.formula: use a tibble of only covariates
#model.matrix(data = tibble_name, ~.)

fit_aft()

ccc %>%
  select(starts_with("covariate"))
