#' Title
#'
#' @param individual_run
#' @param intercepts
#' @param trends
#' @param sigma
#' @param pi
#' @param error_threshold
#'
#' @return
#' @export
#'
#'
#' @importFrom dplyr tibble mutate
#' @importFrom magrittr %>%
#' @importFrom tidyr pivot_longer pivot_wider separate
#'
#'
#' @examples
error_measures_one_run_both_directions <- function(individual_run, intercepts, trends, sigma, pi, error_threshold = Inf){
  if(tail(intercepts, 1) < head(intercepts, 1)){ errorCondition("Incorrect order of intercepts parameter, please start with lower one first")}

  if(length(individual_run) == 1 && individual_run == "Error"){return(tibble(comp = "Error", parameter = "Error", est = "Error", true = "Error", error = "Error", iter = "Error"))}
  else{


    #  a <- min_rank(abs(intercepts - individual_run[[4]]$mean[1]))
    #  b <- min_rank(abs(intercepts - individual_run[[4]]$mean[2])) #this just picks which is closer to truth, might want to change to just run both orders and pick afterwards

    a <- c(1,2)
    b <- c(2,1)

forward  <- tibble(
      comp = c("c1", "c2"),
      est_intercepts = individual_run[[4]]$mean[a],
      true_intercepts = intercepts,
      est_trends = individual_run[[4]]$mean[a + 2],
      true_trends = trends,
      est_sigma = individual_run[[4]]$sd[a],
      true_sigma = sigma,
      est_pi = individual_run[[3]]$`P(C = c)`,
      true_pi = pi) %>%
      mutate(
        error_intercepts = true_intercepts - est_intercepts,
        error_trends = true_trends - est_trends,
        error_sigma = true_sigma - est_sigma,
        error_pi = true_pi - est_pi
      ) %>%
      tidyr::pivot_longer(cols = est_intercepts:error_pi) %>%
      separate(name, sep = "_", into = c("type", "parameter")) %>%
      pivot_wider(names_from = type, values_from = value) %>%
      mutate(iter = individual_run[[5]])


reverse  <- tibble(
    comp = c("c1", "c2"),
    est_intercepts = individual_run[[4]]$mean[b],
    true_intercepts = intercepts,
    est_trends = individual_run[[4]]$mean[b + 2],
    true_trends = trends,
    est_sigma = individual_run[[4]]$sd[b],
    true_sigma = sigma,
    est_pi = individual_run[[3]]$`P(C = c)`,
    true_pi = pi) %>%
    mutate(
      error_intercepts = true_intercepts - est_intercepts,
      error_trends = true_trends - est_trends,
      error_sigma = true_sigma - est_sigma,
      error_pi = true_pi - est_pi
    ) %>%
    tidyr::pivot_longer(cols = est_intercepts:error_pi) %>%
    separate(name, sep = "_", into = c("type", "parameter")) %>%
    pivot_wider(names_from = type, values_from = value) %>%
    mutate(iter = individual_run[[5]])

f_error <- forward %>% summarize(total_error = sum(abs(error)))
f_error <- f_error$total_error
r_error <- reverse %>% summarize(total_error = sum(abs(error)))
r_error <- r_error$total_error

if(min(c(f_error, r_error)) > error_threshold){return(tibble(comp = "Error", parameter = "Error", est = "Error", true = "Error", error = "Error", iter = "Error"))}

    if(f_error < r_error){return(forward)}
    else if(f_error > r_error){return(reverse)}
    else{warningCondition("We have ourselves an issue in determining which version has lower error")
      return(tibble(comp = "Error", parameter = "Error", est = "Error", true = "Error", error = "Error", iter = "Error"))}
  }

}
