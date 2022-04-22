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
sim_data_and_add_covariates <- function(nyears = 5, sample_size_dist = "normal", norm_mean = 100, norm_sd = 10, unif_min, unif_max, covariate_list){

  drawn_observations <- sim_data(nyears, sample_size_dist, norm_mean, norm_sd, unif_min, unif_max) %>%
    ungroup()

  drawn_covariates <- as_tibble(purrr::map(covariate_list, ~draw_covariates(drawn_observations, .x)),  .name_repair = "unique" )

  names(drawn_covariates) <- create_cov_names(drawn_covariates)

  tibble(drawn_observations, drawn_covariates)

}


create_cov_names <- function(df){
  vec = c(1:ncol(df))
  as_vector(map(vec, ~paste("covariate_", .x, sep = "")))
}
