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

  lin_regs <- data_sets %>% rbindlist %>% tibble %>% group_by (iter, comp) %>% group_modify(~ broom::tidy(lm(observed_value ~ t, data = .x))) %>%
    mutate(
      convergence = case_when(
        iter %in% converge_incorrectly_vector ~ "incorrect",
        iter %in% failure_to_converge_vector ~ "failure",
        TRUE ~ "successful"
      ),
      iter_result = paste(iter, convergence, sep = " ")
    ) %>%
    pivot_wider(id_cols = c(iter, comp), names_from = term, values_from = estimate) %>%
    rename(slope = t,
           intercept = `(Intercept)`)

  data <- data_sets %>% rbindlist() %>% tibble %>%
    mutate(
      convergence = case_when(
        iter %in% converge_incorrectly_vector ~ "incorrect",
        iter %in% failure_to_converge_vector ~ "failure",
        TRUE ~ "successful"
      ),
      iter_result = paste(iter, convergence, sep = " ")
    )

    ggplot(data = data) +
    geom_point(aes(x = t, y = observed_value, color = comp), alpha = 0.3) +
    geom_hline(yintercept = c(a, b)) + #change to be variables
    #add trendlines either w/ggplot or map(lm)
    #geom_abline(aes(slope = slope, intercept = intercept), data = lin_regs) +
    facet_wrap(~ iter_result) + geom_smooth(aes(x = t, y = observed_value), method = "lm", color = "darkviolet", size  = 0.5, se = FALSE, data = (data %>% filter(comp == 1))) +
      geom_smooth(aes(x = t, y = observed_value), method = "lm", color = "darkgreen", size  = 0.5, se = FALSE, data = (data %>% filter(comp == 2)))



}
