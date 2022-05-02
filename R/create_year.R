#' create_year
#'
#' @param sample_size_dist
#' @param nyears
#' @param norm_mean
#' @param norm_sd
#' @param unif_min
#' @param unif_max
#'
#' @return
#' @export
#'
#'
#' @examples
create_year <- function(sample_size_dist = "normal", nyears = 5, norm_mean = 50, norm_sd = 10, unif_min, unif_max){
  if(sample_size_dist == "normal"){
    c(rep(0:(nyears - 1), abs(round(rnorm(nyears, norm_mean, norm_sd)))))
  }
  else if(sample_size_dist == "uniform"){
    c(rep(0:(nyears - 1), abs(round(runif(nyears, unif_min, unif_max)))))
  }
  else{warningCondition("invalid choice of distribution, please use either normal or uniform distribution for sample size")}
}
