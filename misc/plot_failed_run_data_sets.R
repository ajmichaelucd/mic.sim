i =c(10, 22, 11, 23)
n = 2000
intercepts = c(-1, 2.3)
trends = c(0.1, 0)
sigma = c(1, 0.5)
pi = c(0.5, 0.5)
nyears = 5
covariate_list <-  NULL
covariate_effect_vector <- c(0)
run_name <- "component_mean_run_4_09272022"
low_con = 2^-3
high_con = 2^2
scale = "log"



data_sets <- purrr::map(i, ~recreate_data_set(i = .x, n = n, intercepts = intercepts, trends = trends, sigma = sigma, pi = pi, nyears = nyears))

library(ggridges)

#map over this function, rbindlist, tibble here
data_sets %>% rbindlist() %>% tibble %>%
  mutate(time_c = floor(t)) %>%
  ggplot() +
  xlim(-3, 3.5)+
  geom_density_ridges(aes(x = observed_value, y = as.character(iter), fill = as.character(iter))) +
  facet_grid(time_c ~ .)

data_sets %>% rbindlist() %>% tibble %>%
  mutate(time_c = floor(t)) %>%
  ggplot() +
  xlim(-3, 3.5) +
  geom_histogram(aes(x = observed_value, fill = as.character(iter))) +
  facet_grid(as.character(iter) ~ time_c)

