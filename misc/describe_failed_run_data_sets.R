
##deprecated, has been integrated into post_test_script


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

##need to get converge_incorrectly_vector and failure_to_converge_vector

df2 <- purrr::map(i, ~describe_data_set(i = .x, n = n, intercepts = intercepts, trends = trends, sigma = sigma, pi = pi, nyears = nyears, converge_incorrectly_vector = converge_incorrectly_vector, failure_to_converge_vector = failure_to_converge_vector, scale = "log"))
df2 %>% rbindlist() %>% tibble() %>%
  group_by(convergence, comp) %>%
  summarise(t = mean(t_avg),
            eps = mean(eps_avg),
            obs_val = mean(obs_val_avg),
            obs_val_adj = mean(obs_val_avg_adj),
            censored_pct = mean(censored_pct)

  ) #not helpful, let's look at % censored

