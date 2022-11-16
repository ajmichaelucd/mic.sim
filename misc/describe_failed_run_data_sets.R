i = 1:1000
n = 2000
intercepts = c(-1, 2.3)
trends = c(0.1, 0)
sigma = c(1, 0.5)
pi = c(0.5, 0.5)
nyears = 5
covariate_list <-  NULL
covariate_effect_vector <- c(0)
run_name <- "component_mean_run_7_09272022"
low_con = 2^-3
high_con = 2^2
scale = "log"

describe_data_set <- function(i, n, intercepts, trends, sigma, pi, nyears, converge_incorrectly_vector, failure_to_converge_vector){
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
    scale = "log")

  data.sim %>% mutate(iter = i,
                      censored = case_when(indicator == 3 ~ 0,
                                           TRUE ~ 1)) %>%
  #  rowwise() %>%
    mutate(obs_val_adj = observed_value - (trends[as.numeric(comp)] * t)) %>%  #scaled to be what if they were all time = 1
    group_by(comp) %>%
    summarise( t_avg = mean(t),
               eps_avg = mean(epsilon),
               obs_val_avg = mean(observed_value),
               obs_val_avg_adj = mean(observed_value - (trends[as.numeric(comp)] * t)),
               censored_pct = sum(censored)/n) %>%
    rowwise() %>%
    mutate(iter = i,
           convergence = case_when(
             iter %in% converge_incorrectly_vector ~ "incorrect",
             iter %in% failure_to_converge_vector ~ "failure",
             TRUE ~ "successful"
           )
             )
}


df2 <- purrr::map(i, ~describe_data_set(i = .x, n = n, intercepts = intercepts, trends = trends, sigma = sigma, pi = pi, nyears = nyears, converge_incorrectly_vector = converge_incorrectly_vector, failure_to_converge_vector = failure_to_converge_vector))
df2 %>% rbindlist() %>% tibble() %>%
  group_by(convergence, comp) %>%
  summarise(t = mean(t_avg),
            eps = mean(eps_avg),
            obs_val = mean(obs_val_avg),
            obs_val_adj = mean(obs_val_avg_adj),
            censored_pct = mean(censored_pct)

  ) #not helpful, let's look at % censored

