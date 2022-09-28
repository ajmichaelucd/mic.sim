#' error_measures_one_run
#'
#' Extracts error measures from a single simulation. Useful when used in a map function applied to a batch of simulations.
#'
#' @param individual_run
#' @param intercepts
#' @param trends
#' @param sigma
#'
#' @return
#' @export
#'
#' @importFrom dplyr tibble mutate
#' @importFrom magrittr %>%
#' @importFrom tidyr pivot_longer pivot_wider separate
#'
#' @examples
error_measures_one_run <- function(individual_run, intercepts, trends, sigma){
  if(tail(intercepts, 1) < head(intercepts, 1)){ errorCondition("Incorrect order of intercepts parameter, please start with lower one first")}

  if(length(individual_run) == 1 && individual_run == "Error"){return(tibble(comp = "Error", parameter = "Error", est = "Error", true = "Error", error = "Error", iter = "Error"))}
  else{


#  a <- min_rank(abs(intercepts - individual_run[[4]]$mean[1]))
#  b <- min_rank(abs(intercepts - individual_run[[4]]$mean[2])) #this just picks which is closer to truth, might want to change to just run both orders and pick afterwards

  a <- min_rank(abs(intercepts - individual_run[[4]][[1]][1]))
  b <- min_rank(abs(intercepts - individual_run[[4]][[1]][2]))

  errorCondition(rev(b) != a, "Issue with identifying means")

  tibble(
    comp = c("c1", "c2"),
    est_intercepts = individual_run[[4]]$mean[a],
    true_intercepts = intercepts,
    est_trends = individual_run[[4]]$mean[a + 2],
    true_trends = trends,
    est_sigma = individual_run[[4]]$sd[a],
    true_sigma = sigma) %>%
    mutate(
      error_intercepts = true_intercepts - est_intercepts,
      error_trends = true_trends - est_trends,
      error_sigma = true_sigma - est_sigma
    ) %>%
    tidyr::pivot_longer(cols = est_intercepts:error_sigma) %>%
    separate(name, sep = "_", into = c("type", "parameter")) %>%
    pivot_wider(names_from = type, values_from = value) %>%
    mutate(iter = individual_run[[5]])

  }

}
