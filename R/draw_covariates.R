


#' draw_covariates
#'
#' draw_covariates function uses the draw_categorical_covariate and draw_numerical_covariate to add covariates to a dataframe or tibble of drawn observations
#'
#' the cov_list is a list object of vectors with a vector for each covariate to be added, either numeric or categorical
#' the first value in each vector should be either "numeric" or "categorical"
#'
#' for numeric variables, the second value should specify whether it is drawn from a normal or uniform distribution using "normal" or "uniform"
#' the next two values are the mean and sd if drawn from a normal distribution or min and max if drawn from a uniform distribution
#'
#' for categorical variables, the remaining values in the vector will be probabilities for each level of the variable, which will be labeled as a, b, c, and so on
#'
#' example:
#' cov_list <- list(
#' c("numeric", "normal", 40, 4),
#' c("categorical", 0.3, 0.4, 0.3),
#' c("categorical", 0.5, 0.2, 0.3),
#' c("categorical", 0.2, 0.2, 0.2, 0.2, 0.2),
#' c("numeric", "uniform", 1, 10))
#'
#' @param input
#' @param cov_list
#'
#'
#' @return
#' @keywords internal
#'
#' @examples
draw_covariates <- function(input, cov_list){
  if(cov_list[1] == "numeric"){draw_numerical_covariate(input, cov_list)}
  else if(cov_list[1] == "categorical"){draw_categorical_covariate(input, cov_list)}
  else{errorCondition("Invalid type, pick either numeric or categorical")}
}


draw_categorical_covariate <- function(input, covariate_list_vector){
  categorical_covariate_probability_vector <- as.numeric(covariate_list_vector[-1])
  sample(c(letters[1:length(categorical_covariate_probability_vector)]), size = nrow(input), replace = TRUE, prob = categorical_covariate_probability_vector)
}

draw_numerical_covariate <- function(input, numerical_variable_vector){
  if(
    numerical_variable_vector[2] == "normal"){
    rnorm(nrow(input), mean = as.numeric(numerical_variable_vector[3]), sd = as.numeric(numerical_variable_vector[4]))
  }
  else if(numerical_variable_vector[2] == "uniform"){
    runif(nrow(input), min = as.numeric(numerical_variable_vector[3]), max = as.numeric(numerical_variable_vector[4]))
  }
  else{warningCondition("Invalid distribution, choose uniform or normal")}
}


