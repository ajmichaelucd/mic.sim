#' component_mean - alternate version
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
component_mean_alt = function(
    n = 100,
  t_dist = function(n){runif(n, min = 0, max = 1)},
  pi = function(t)
  {
    z <- 0.5 + 0.2 * t
    c("1" = z, "2" = 1- z)
  },
  `E[X|T,C]` = function(t, c)
  {
    case_when(
      c == "1" ~ 3 + t + 2*t^2 - sqrt(t),
      c == "2" ~ 3*t,
      TRUE ~ NA
    )
  }
)
{
  temp =
    tibble(
      t = t_dist(n = n),
      p = map(t, ~ pi(.x)),
      comp = gen_comp(p),
      x = `E[X|T,C]`(t, comp)
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
