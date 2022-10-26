#' report_failure_types
#'
#' @param array_results
#'
#' @return
#' @export
#'
#' @importFrom data.table rbindlist
#' @importFrom dplyr tibble mutate select distinct count filter pull
#' @importFrom magrittr %>%
#'
#' @examples
report_failure_types <- function(array_results){
  bbb <- array_results %>% rbindlist() %>% tibble() %>% mutate(converge_fail = ifelse(comp == "Error", TRUE, FALSE)) %>%  select(-(comp:error)) %>% distinct()

  iters = nrow(bbb)

  converge_fail <- bbb %>% select(converge_fail) %>%  count(converge_fail) %>% filter(converge_fail) %>% pull(n)
  sigma_error <- bbb %>% select(sigma_error) %>% mutate(sigma_error = as.logical(sigma_error)) %>%  count(sigma_error) %>% filter(sigma_error) %>% pull(n)
  pi_error <- bbb %>% select(pi_error) %>% mutate(pi_error = as.logical(pi_error)) %>% count(pi_error) %>% filter(pi_error) %>% pull(n)
  intercept_error <- bbb %>% select(intercept_error) %>% mutate(intercept_error = as.logical(intercept_error)) %>% count(intercept_error) %>% filter(intercept_error) %>% pull(n)
  trends_error <- bbb %>% select(trends_error) %>% mutate(trends_error = as.logical(trends_error)) %>% count(trends_error) %>% filter(trends_error) %>% pull(n)

  tibble(converge_fail = converge_fail / iters, sigma_error = sigma_error / iters, pi_erro = pi_error / iters, intercept_error = intercept_error / iters, trends_error = trends_error / iters)
}
