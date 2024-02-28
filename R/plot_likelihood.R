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
      rename(step = V1, loglikelihood = V2, survreg_maxout = V3)
    diff = c(NaN, diff(like$loglikelihood))
      like = like %>% tibble(., diff) %>%
      filter(!is.na(loglikelihood)) }
  else if(format == "tibble"){
    diff = c(NaN, diff(likelihood_documentation$loglikelihood))
    like = likelihood_documentation %>% tibble(., diff) %>%
      filter(!is.na(loglikelihood))
  }

  like %>%
    mutate(
            diff_sign =
              case_when(diff > 0 ~ "inc",
                        is.na(diff) ~ NA,
                        TRUE ~ "dec")
    ) %>%
    ggplot() +
    geom_line(aes(x = step, y = loglikelihood)) +
    geom_point(aes(x = step, y = loglikelihood, color = diff_sign))
}
