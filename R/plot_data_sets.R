#' plot_data_sets
#'
#' @param data_sets
#' @param converge_incorrectly_vector
#' @param failure_to_converge_vector
#'
#' @return
#' @export
#'
#' @importFrom ggplot2 ggplot geom_point xlim aes geom_hline facet_wrap
#' @importFrom dplyr mutate tibble case_when
#' @importFrom data.table rbindlist
#'
#' @examples
plot_data_sets <- function(data_sets, converge_incorrectly_vector, failure_to_converge_vector, low_con, high_con){
  a <- log2(low_con)
  b <- log2(high_con)



  data_sets %>% rbindlist() %>% tibble %>%
    mutate(
      convergence = case_when(
        iter %in% converge_incorrectly_vector ~ "incorrect",
        iter %in% failure_to_converge_vector ~ "failure",
        TRUE ~ "successful"
      ),
      iter_result = paste(iter, convergence, sep = " ")
    ) %>%
    ggplot() +
    geom_point(aes(x = t, y = observed_value, color = comp), alpha = 0.3) +
    geom_hline(yintercept = c(a, b)) + #change to be variables
    #add trendlines either w/ggplot or map(lm)
    facet_wrap(~ iter_result)

}
