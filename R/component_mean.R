#' component_mean
#'
#' @param n
#' @param t_dist
#' @param pi
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
  t_dist = function(n){runif(n, min = 0, max = 1)},
  pi = function(t) {z <- 0.5 + 0.2 * t
                    c("1" = z, "2" = 1- z)},
  complist = list(
    "1" = function(t) {3 + t + 2*t^2 -sqrt(t)},
    "2" = function(t) {3*t}
  ))
{
  temp =
    tibble(t = t_dist(n = n),
      p = map(t, ~ pi(.x)),
      comp = gen_comp(p)) %>%
    group_by(comp)

  temp2 = split(temp, f = temp$comp)

  temp3 = purrr::map_dfr(
    .x = names(temp2),
    .f = function(x)
    {
      temp2[[x]] %>%
        mutate(x = complist[[x]](t))

    }
  )

}

gen_comp = function(p)
{
  purrr::map_chr(p, ~ sample(
    x = names(.x),
    size = 1,
    prob = .x,
    replace = TRUE
  ))
}
