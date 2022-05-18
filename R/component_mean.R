#' component_mean
#'
#' @param n
#' @param t_dist
#' @param pi
#' @param mean_func
#' @param complist
#'
#' @return
#' @export
#'
#' @importFrom dplyr tibble mutate inner_join
#' @importFrom magrittr %>%
#' @importFrom purrr map as_vector
#'
#' @examples
component_mean = function(
    n = 100,
    t_dist = t_dist1,
    pi = pi1,
    mean_func = mean_func1,
    complist = complist1)
{

  tibble(
    t = t_dist(n = n),
    p = map(t, ~pi(.x)),
    comp = as_vector(gen_comp(t, p)),
    x = mean_func(t = t, comp = comp, complist = complist))
}

gen_comp = function(t, p)
{
  map(p, ~sample.int(n = length(.x), size = 1, prob = .x, replace = TRUE))
}
