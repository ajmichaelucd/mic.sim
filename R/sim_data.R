
#' sim_data
#'
#' Function to draw values for each year
#'
#' @param nyears
#' @param sample_size_dist
#' @param norm_mean
#' @param norm_sd
#' @param unif_min
#' @param unif_max
#' @param initial_lower_mean
#' @param initial_upper_mean
#' @param initial_lower_std_dev
#' @param initial_upper_std_dev
#' @param initial_lower_slp
#' @param initial_upper_slp
#' @param initial_lower_slp_qd
#' @param initial_upper_slp_qd
#' @param initial_lower_lambda
#' @param initial_upper_lambda
#' @param initial_lower_slp_lambda
#' @param initial_upper_slp_lambda
#'
#' @return
#' @export
#'
#' @examples
sim_data <- function(nyears = 5, sample_size_dist = "normal", norm_mean = 100, norm_sd = 10, unif_min, unif_max,
                     initial_lower_mean = -1, initial_upper_mean = 1, initial_lower_std_dev = 1, initial_upper_std_dev = 1, initial_lower_slp = 0, initial_upper_slp = 0, initial_lower_slp_qd = 0, initial_upper_slp_qd = 0,
                     initial_lower_lambda = 0.5, initial_upper_lambda = 0.5, initial_lower_slp_lambda = 0, initial_upper_slp_lambda = 0){ ##ADD THE REST OF THIS STUFF IN HERE
  if(sample_size_dist == "normal"){
    tibble(year = rep(0:(nyears - 1), abs(round(rnorm(nyears, norm_mean, norm_sd))))) %>%
      rowwise() %>%
      mutate(base_value = draw_observations(year, initial_lower_mean, initial_upper_mean, initial_lower_std_dev, initial_upper_std_dev, initial_lower_slp, initial_upper_slp, initial_lower_slp_qd, initial_upper_slp_qd,
                                                initial_lower_lambda, initial_upper_lambda, initial_lower_slp_lambda, initial_upper_slp_lambda)
      )
  }
  else if(sample_size_dist == "uniform"){
    tibble(year = rep(0:(nyears - 1), abs(round(runif(nyears, unif_min, unif_max))))) %>%
      rowwise() %>%
      mutate(base_value = draw_observations(year, initial_lower_mean, initial_upper_mean, initial_lower_std_dev, initial_upper_std_dev, initial_lower_slp, initial_upper_slp, initial_lower_slp_qd, initial_upper_slp_qd,
                                                initial_lower_lambda, initial_upper_lambda, initial_lower_slp_lambda, initial_upper_slp_lambda)
      )
  }
  else{warningCondition("invalid choice of distribution, please use either normal or uniform distribution for sample size")}

}
