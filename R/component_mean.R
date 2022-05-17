#' component_mean
#'
#' @param n
#' @param t_dist
#' @param pi
#' @param mean_func
#'
#' @return
#' @export
#'
#' @examples
component_mean = function(
    n = 100,
    t_dist = t_dist1,
    pi = pi1,
    mean_func = mean_func1)
{

  tibble(
    t = t_dist(n = n),
    p = map(t, ~pi(.x)),
    comp = as_vector(gen_comp(t, p)),
    x = mean_func(t = t, comp = comp))
}

gen_comp = function(t, p)
{
  map(p, ~sample.int(n = length(.x), size = 1, prob = .x, replace = TRUE))
}
