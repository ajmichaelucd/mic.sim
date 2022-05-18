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
                                 t_dist = t_dist1,
                                 pi = pi1,
                                 mean_func = mean_func1,
                                 complist = complist1,
                                 sd_vector = c("1" = 1, "2" = 2)){
  component_mean(n, t_dist, pi, mean_func, complist) %>%
    mutate(sd = sd_vector[comp]) %>%
    mutate(epsilon = rnorm(length(t), x, sd))
}
