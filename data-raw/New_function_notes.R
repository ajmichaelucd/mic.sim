A <- tibble( y  = c(0, 1, 2, 3), x1 = c(0, 0, 1, 1))
B <- tibble(x2 = c(1,1,2,3))
lm(y~x1 + B$x2, data = A)

complist1 = list(
  f1 = function(t) {3 + t + 2*t^2 -sqrt(t)},
  f2 = function(t) {3*t}
)

pi = function(t) {z <- 0.5 + 0.2 * t
c(z, 1- z)}



mean_func1 = function(comp, complist, t)
{
  case_when(
    comp == 1 ~ complist[[1]](t),
    comp == 2 ~ complist[[2]](t))
}

gen_comp = function(t, p)
{
  map(p, ~sample.int(n = length(.x), size = 1, prob = .x, replace = TRUE))
}

t_dist1 = function(n)
{
  runif(n, min = 0, max = 1)
}

component_mean = function(
    n = 100,
    t_dist = t_dist1,
    pi = pi1,
    mean_func = mean_func1)
{

  tibble(
    t = t_dist(n = n),
    p = map(t, ~pi(.x)),
    comp = as_vector(gen_comp(t, p)),
    x = mean_func(t = t, comp = comp))
}
component_mean()
sd = c(1, 2)

draw_observed_values <- function(n = 100,
                                 t_dist = t_dist1,
                                 pi = pi1,
                                 mean_func = mean_func1,
                                 comp_func = gen_comp,
                                 sd_vector = c("1" = 1, "2" = 2)){
  component_mean(n, t_dist, pi, mean_func, comp_func) %>%
  mutate(sd = sd_vector[comp]) %>%
  mutate(epsilon = rnorm(length(t), x, sd))

}
draw_observed_values()
  #draw_observed_values(n, t_dist, pi, mean_func, comp_func, sd_vector)


  simulate_mics2 <- function(n = 100,
                            t_dist = t_dist1,
                            pi = pi1,
                            mean_func = mean_func1,
                            comp_func = gen_comp,
                            sd_vector = c(1,1),
                            covariate_list,
                            covariate_effect_vector,
                            low_con = 2^-4,
                            high_con = 2^4,
                            tested_concentrations = log2(low_con):log2(high_con)){
    #base_data <- find_epsilon(year, sd_1, sd_2, mean_1_trend, mean_2_trend, mean_1_intercept, mean_2_intercept, pi_1_trend, pi_1_intercept)
    base_data <- draw_observed_values(n, t_dist, pi, mean_func, comp_func, sd_vector)
    covariate_data <- add_covariate(covariate_list = covariate_list, input = base_data$t)
    merged_data <- tibble(base_data, covariate_data)
    total_cov_effect <- covariate_effect_total(merged_data, covariate_effect_vector)
    simulated_obs <- tibble(merged_data, total_cov_effect) %>%
      mutate(observed_value = epsilon + total_cov_effect)
    censored_obs <- censor_values(simulated_obs$observed_value, low_con, high_con, tested_concentrations)
    inner_join(simulated_obs, censored_obs)
  }
simulate_mics2(covariate_list = covariate_list,
               covariate_effect_vector = covariate_effect_vector)
