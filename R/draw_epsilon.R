#' draw_epsilon
#'
#' @param n
#' @param t_dist
#' @param pi
#' @param mean_func
#' @param complist
#' @param sd_vector
#'
#' @return
#' @export
#'
#' @importFrom dplyr tibble mutate inner_join
#' @importFrom magrittr %>%
#' @importFrom purrr map as_vector
#'
#' @examples
draw_epsilon <- function(n = 100,
                                 t_dist = function(n){runif(n, min = 0, max = 1)},
                                 pi = function(t) {z <- 0.5 + 0.2 * t
                                 c("1" = z, "2" = 1- z)},
                                 complist = list(
                                   "1" = function(t) {3 + t + 2*t^2 -sqrt(t)},
                                   "2" = function(t) {3*t}
                                 ),
                                 sd_vector = c("1" = 1, "2" = 2)){
  component_mean(n, t_dist, pi, complist) %>%
    mutate(sd = sd_vector[comp]) %>%
    mutate(epsilon = rnorm(length(t), 0, sd))
}