i = c(10, 22, 2, 7, 1, 3)
n = 2000
intercepts = c(-1, 2.3)
trends = c(0.1, 0)
sigma = c(1, 0.5)
pi = c(0.5, 0.5)
nyears = 5
covariate_list <-  NULL
covariate_effect_vector <- c(0)
run_name <- "component_mean_run_8_09272022"
low_con = 2^-3
high_con = 2^2
scale = "log"

data_sets <- purrr::map(i, ~recreate_data_set(i = .x, n = n, intercepts = intercepts, trends = trends, sigma = sigma, pi = pi, nyears = nyears))



library(ggplot2)

#map over this function, rbindlist, tibble here
data_sets %>% rbindlist() %>% tibble %>%
  mutate(time_c = floor(t)) %>%
  ggplot() +
  xlim(-3, 3.5)+
  geom_density_ridges(aes(x = observed_value, y = as.character(iter), fill = as.character(iter))) +
  facet_grid(time_c ~ .)

data_sets %>% rbindlist() %>% tibble %>%
  mutate(
         convergence = case_when(
           iter %in% converge_incorrectly_vector ~ "incorrect",
           iter %in% failure_to_converge_vector ~ "failure",
           TRUE ~ "successful"
         )
  ) %>%
  ggplot() +
  geom_point(aes(x = t, y = observed_value, color = comp), alpha = 0.3) +
  geom_hline(yintercept = c(-3, 2)) + #change to be variables
  #add trendlines either w/ggplot or map(lm)
  facet_wrap(~ iter)

