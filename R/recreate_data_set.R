#' recreate_data_set
#'
#' @param i
#' @param n
#' @param intercepts
#' @param trends
#' @param sigma
#' @param pi
#' @param nyears
#' @param low_con
#' @param high_con
#' @param scale
#'
#' @return
#' @export
#'
#' @examples
recreate_data_set <- function(i, n, intercepts, trends, sigma, pi, nyears, low_con = 2^-3, high_con = 2^2, scale = "log"){
  set.seed(i)
  t_dist1 = function(n)
  {runif(n, min = 0, max = nyears)}
  pi1 = function(t)
  {z <- pi[1] #changed to 0.5
  c("1" = z, "2" = 1- z)}
  `E[X|T,C]` = function(t, c)
  {
    case_when(
      c == "1" ~ intercepts[1] + trends[1] * t,
      c == "2" ~ intercepts[2] + trends[2] * t, #1, 1.5, 1.75, 1.9, 2, 2.1, 2.2, 2.3
      TRUE ~ NaN
    )
  }
  sd_vector = c("1" = sigma[1], "2" = sigma[2]) #0.5, 0.75, 1, 1.25



  data.sim <- simulate_mics(
    n = n,
    t_dist = t_dist1,
    pi = pi1,
    `E[X|T,C]` = `E[X|T,C]`,
    sd_vector = sd_vector,
    covariate_list = covariate_list,
    covariate_effect_vector = covariate_effect_vector,
    low_con = low_con,
    high_con = high_con,
    scale = scale)

  data.sim %>% mutate(iter = i)
}
