#' Title
#'
#' @param nyears
#' @param sample_size_dist
#' @param norm_mean
#' @param norm_sd
#' @param unif_min
#' @param unif_max
#' @param covariate_list
#'
#' @return
#' @export
#'
#'
#' @importFrom purrr map as_vector
#' @importFrom dplyr as_tibble ungroup mutate rowwise tibble
#' @importFrom magrittr %>%
#'
#' @examples
sim_data_and_add_covariates <- function(nyears, sample_size_dist, norm_mean, norm_sd, unif_min, unif_max, covariate_list){

  drawn_observations <- sim_data(nyears, sample_size_dist, norm_mean, norm_sd, unif_min, unif_max) %>%
    ungroup()

  drawn_covariates <- as_tibble(purrr::map(covariate_list, ~draw_covariates(drawn_observations, .x)),  .name_repair = "unique" )

  names(drawn_covariates) <- create_cov_names(drawn_covariates)

  tibble(drawn_observations, drawn_covariates)

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


draw_covariates <- function(input, cov_list){
  if(cov_list[1] == "numeric"){draw_numerical_covariate(input, cov_list)}
  else if(cov_list[1] == "categorical"){draw_categorical_covariate(input, cov_list)}
  else{errorCondition("Invalid type, pick either numeric or categorical")}
}


create_cov_names <- function(df){
  vec = c(1:ncol(df))
  as_vector(map(vec, ~paste("covariate_", .x, sep = "")))
}
