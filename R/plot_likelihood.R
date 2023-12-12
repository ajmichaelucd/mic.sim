#' Title
#'
#' @param likelihood_documentation
#' @param format
#'
#' @import ggplot2
#'
#' @return
#' @export
#'
#' @examples
plot_likelihood = function(likelihood_documentation, format = "tibble"){
  if(format == "matrix"){
    like <- likelihood_documentation %>%
      as_tibble %>%
      suppressWarnings() %>%
      rename(step = V1, likelihood = V2, survreg_maxout = V3)
    diff = c(NaN, diff(like$likelihood))
      like = like %>% tibble(., diff) %>%
      filter(!is.na(likelihood)) }
  else if(format == "tibble"){
    diff = c(NaN, diff(likelihood_documentation$likelihood))
    like = likelihood_documentation %>% tibble(., diff) %>%
      filter(!is.na(likelihood))
  }

  like %>%
    mutate(
            diff_sign =
              case_when(diff > 0 ~ "inc",
                        is.na(diff) ~ NA,
                        TRUE ~ "dec")
    ) %>%
    ggplot() +
    geom_line(aes(x = step, y = likelihood)) +
    geom_point(aes(x = step, y = likelihood, color = diff_sign))
}
