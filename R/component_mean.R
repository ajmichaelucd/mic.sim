#' component_mean
#'
#' @param n
#' @param t_dist
#' @param pi
#' @param `E[X|T,C]`
#'
#' @return
#' @export
#'
#' @importFrom dplyr tibble mutate inner_join case_when
#' @importFrom magrittr %>%
#' @importFrom purrr map as_vector map_chr
#'
#' @examples
component_mean = function(
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
        TRUE ~ NaN
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
