#' describe_data_set
#'
#' @param i
#' @param n
#' @param intercepts
#' @param trends
#' @param sigma
#' @param pi
#' @param nyears
#' @param converge_incorrectly_vector
#' @param failure_to_converge_vector
#' @param scale
#'
#' @return
#' @export
#'
#' @importFrom dplyr full_join case_when mutate group_by summarise rowwise filter tibble
#' @importFrom magrittr %>%
#'
#'
#' @examples
describe_data_set <- function(i, n, intercepts, trends, sigma, pi, nyears, converge_incorrectly_vector, failure_to_converge_vector, scale = "log"){
  set.seed(i)
  t_dist1 = function(n)
  {runif(n, min = 0, max = nyears)}
  pi1 = function(t)
  {z <- pi[1] #changed to 0.5
  c("1" = z, "2" = 1- z)}
  `E[X|T,C]` = function(t, c)
  {
    case_when(
      c == "1" ~ intercepts[1] + trends[1] * t,
      c == "2" ~ intercepts[2] + trends[2] * t, #1, 1.5, 1.75, 1.9, 2, 2.1, 2.2, 2.3
      TRUE ~ NaN
    )
  }
  sd_vector = c("1" = sigma[1], "2" = sigma[2]) #0.5, 0.75, 1, 1.25



  data.sim <- simulate_mics(
    n = n,
    t_dist = t_dist1,
    pi = pi1,
    `E[X|T,C]` = `E[X|T,C]`,
    sd_vector = sd_vector,
    covariate_list = covariate_list,
    covariate_effect_vector = covariate_effect_vector,
    low_con = low_con,
    high_con = high_con,
    scale = scale)

  summarized_data <- data.sim %>% mutate(iter = i,
                      censored = case_when(indicator == 3 ~ 0,
                                           TRUE ~ 1)) %>%
    #  rowwise() %>%
    mutate(obs_val_adj = observed_value - (trends[as.numeric(comp)] * t)) %>%  #scaled to be what if they were all time = 1
    group_by(comp) %>%
    summarise( t_avg = mean(t),
               eps_avg = mean(epsilon),
               obs_val_avg = mean(observed_value),
               obs_val_avg_adj = mean(observed_value - (trends[as.numeric(comp)] * t)),
               censored_pct = sum(censored)/n,
               pi_pct = n()/ n) %>%
    rowwise() %>%
    mutate(iter = i,
           convergence = case_when(
             iter %in% converge_incorrectly_vector ~ "incorrect",
             iter %in% failure_to_converge_vector ~ "failure",
             TRUE ~ "successful"
           )
    )

  a <- data.sim %>% filter(comp == 1)
  b <- data.sim %>% filter(comp == 2)
  c <- lm(observed_value ~ t, data = a)
  d <-  lm(observed_value ~ t, data = b)
  gg <- tibble(comp = as.character(1:2), intercepts = c(c$coefficients[1], d$coefficients[1]), trends = c(c$coefficients[2], d$coefficients[2]))

  full_join(summarized_data, gg, by = 'comp')

}
