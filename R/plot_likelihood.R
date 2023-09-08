#' Title
#'
#' @param likelihood_documentation
#' @param format
#'
#' @return
#' @export
#'
#' @examples
plot_likelihood = function(likelihood_documentation, format){
  if(format == "matrix"){
    like <- likelihood_documentation %>%
      as_tibble %>%
      suppressWarnings() %>%
      rename(step = V1, likelihood = V2, survreg_maxout = V3) %>%
      tibble(., diff = c(NaN, diff(.$likelihood))) %>%
      filter(!is.na(likelihood)) }
  else if(format == "tibble"){
    like = likelihood_documentation
  }

  like %>%
    mutate( diff_sign =
              case_when(diff > 0 ~ "inc",
                        is.na(diff) ~ NA,
                        TRUE ~ "dec")
    ) %>%
    ggplot() +
    geom_line(aes(x = step, y = likelihood)) +
    geom_point(aes(x = step, y = likelihood, color = diff_sign))
}
