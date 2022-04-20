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
#'
#' @return
#' @export
#'
#' @examples
sim_data <- function(nyears, sample_size_dist, norm_mean, norm_sd, unif_min, unif_max){ ##ADD THE REST OF THIS STUFF IN HERE
  if(sample_size_dist == "normal"){
    tibble(year = rep(0:(nyears - 1), abs(round(rnorm(nyears, norm_mean, norm_sd))))) %>%
      rowwise() %>%
      mutate(observed_value = draw_observations(year)
      )
  }
  else if(sample_size_dist == "uniform"){
    tibble(year = rep(0:(nyears - 1), abs(round(runif(nyears, unif_min, unif_max))))) %>%
      rowwise() %>%
      mutate(observed_value = draw_observations(year)
      )
  }
  else{warningCondition("invalid choice of distribution, please use either normal or uniform distribution for sample size")}

}
