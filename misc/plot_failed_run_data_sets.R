load_all()

set_numbers = c(10, 22, 2, 7, 1, 3)
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


recreate_and_plot_data(set_numbers, n, intercepts, trends, sigma, pi, nyears, covariate_list, covariate_effect_vector, low_con, high_con, scale, converge_incorrectly_vector, failure_to_converge_vector)


#
# #map over this function, rbindlist, tibble here
# data_sets %>% rbindlist() %>% tibble %>%
#   mutate(time_c = floor(t)) %>%
#   ggplot() +
#   xlim(-3, 3.5)+
#   geom_density_ridges(aes(x = observed_value, y = as.character(iter), fill = as.character(iter))) +
#   facet_grid(time_c ~ .)

data_sets %>% rbindlist %>% tibble %>% group_by (iter, comp) %>% group_modify(~ broom::tidy(lm(observed_value ~ t, data = .x)))






