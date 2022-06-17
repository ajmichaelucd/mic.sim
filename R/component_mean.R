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
  t_dist = t_dist1,
  pi = pi1,
  complist = complist1)
{
  temp =
    tibble(t = t_dist(n = n),
      p = map(t, ~ pi(.x)),
      comp = gen_comp(p)) |>
    group_by(comp)

  temp2 = split(temp, f = temp$comp)

  temp3 = purrr::map_dfr(
    .x = names(temp2),
    .f = function(x)
    {
      temp2[[x]] |>
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
