random_seeds_vector = sample(x = 1:1000000, size = 20, replace = FALSE)

set.seed(1)
n = 300
ncomp = 2
pi = function(t) {
  z <- 0.07 + 0.03 * t - 0.00045 * t^2
  #z <- (1+ exp(-m))^-1 #if exp(m) gets large, it won't add the 1 so we write like this
  tibble("1" = 1 - z, "2" = z)
}
`E[X|T,C]` = function(t, c)
{
  case_when(
    c == "1" ~ -4.0 + (0.24 * t) - (0.0055 * t^2),
    c == "2" ~ 3 + 0.1 * t,
    TRUE ~ NaN
  )
}
t_dist = function(n){runif(n, min = 0, max = 16)}
attr(t_dist, "min") = 0
attr(t_dist, "max") = 16
sd_vector = c("1" = 1, "2" = 1.05)
low_con = -3
high_con = 3
scale = "log"
example_data = simulate_mics(n = n, t_dist = t_dist, pi = pi, `E[X|T,C]` = `E[X|T,C]`, sd_vector = sd_vector, covariate_list = NULL, covariate_effect_vector = c(0), low_con = low_con, high_con = high_con, scale = "log") %>% suppressMessages()

example_data %>%
  mutate(verbose_comp = case_when(
    comp == 1 ~ "Component 1",
    TRUE ~ "Component 2"
  )) %>%
  ggplot() +
  geom_segment(aes(x = t, xend = t, y = left_bound, yend = right_bound, color = verbose_comp), data = (. %>% filter(left_bound != -Inf & right_bound != Inf)), alpha = 0.3) +
  geom_segment(aes(x = t, xend = t, y = right_bound, yend = left_bound, color = verbose_comp), data = (. %>% filter(left_bound == -Inf) %>% mutate(left_bound = low_con - 2)), arrow = arrow(length = unit(0.03, "npc")), alpha = 0.3) +
  geom_segment(aes(x = t, xend = t, y = left_bound, yend = right_bound, color = verbose_comp), data = (. %>% filter(right_bound == Inf) %>% mutate(right_bound = high_con + 2)), arrow = arrow(length = unit(0.03, "npc")), alpha = 0.3) +
  geom_point(aes(x = t, y = left_bound, color = verbose_comp), data = . %>% filter(left_bound != -Inf), alpha = 0.3) +
  geom_point(aes(x = t, y = right_bound, color = verbose_comp), data = . %>% filter(right_bound != Inf), alpha = 0.3) +
  geom_function(fun = function(t){`E[X|T,C]`(t, c = "1")}, aes(color = "Component 1 Mean")) +
  geom_function(fun = function(t){`E[X|T,C]`(t, c = "2")}, aes(color = "Component 2 Mean")) +
  xlim(attr(t_dist, "min") ,attr(t_dist, "max")) +
  ylim(low_con - 2, high_con + 2)


#map over the random seeds
visible_data = example_data
mu_formula = Surv(time = left_bound, time2 = right_bound, type = "interval2") ~ pspline(t, df = 0, caic = TRUE)
pi_formula = c == "2" ~ s(t)
max_it = 3000
ncomp = 2
tol_ll = 1e-6
pi_link = "logit"
model_coefficient_tolerance = 0.00001
maxiter_survreg = 30
initial_weighting = 7




run = function(seed, iter, visible_data, mu_formula, pi_formula, max_it, ncomp, tol_ll, pi_link, verbose, initial_weighting, sd_initial){

  EM = purrr::possibly(.f = EM_algorithm_surv, otherwise = "Error")

  single_model_output <- visible_data %>%
    EM(visible_data = visible_data,
       mu_formula = mu_formula,
       pi_formula = pi_formula,
       max_it = max_it,
       ncomp = ncomp,
       tol_ll = tol_ll,
       pi_link = pi_link,
       verbose = verbose,
       initial_weighting = initial_weighting,
       browse_each_step = FALSE,
       plot_visuals = FALSE,
       stop_on_likelihood_drop = FALSE,
       pause_on_likelihood_drop = FALSE,
       model_coefficient_tolerance = model_coefficient_tolerance,
       maxiter_survreg = maxiter_survreg,
       sd_initial = sd_initial,
       seed = seed,
       randomize = "sigma"
    )

  if(length(single_model_output) > 1){
    final_like = single_model_output$likelihood %>% select(step, likelihood) %>% tail(1) %>% mutate(iter = iter, seed = seed, converge = single_model_output$converge)
  }else{
    final_like = tibble(step = NA_integer_, likelihood = NA_integer_, iter = iter, seed = seed, converge = "Error")
  }
  list(output = single_model_output, seed = seed, iter = iter, final_like = final_like)

}

grid_output_0.1 = map2(random_seeds_vector, 1:length(random_seeds_vector), ~ run(.x, .y, visible_data, mu_formula, pi_formula, max_it, ncomp, tol_ll, pi_link, verbose, initial_weighting, sd_initial = 0.1), progress = TRUE)
grid_output_0.2 = map2(random_seeds_vector, 1:length(random_seeds_vector), ~ run(.x, .y, visible_data, mu_formula, pi_formula, max_it, ncomp, tol_ll, pi_link, verbose, initial_weighting, sd_initial = 0.2), progress = TRUE)
grid_output_0.3 = map2(random_seeds_vector, 1:length(random_seeds_vector), ~ run(.x, .y, visible_data, mu_formula, pi_formula, max_it, ncomp, tol_ll, pi_link, verbose, initial_weighting, sd_initial = 0.3), progress = TRUE)
grid_output_0.4 = map2(random_seeds_vector, 1:length(random_seeds_vector), ~ run(.x, .y, visible_data, mu_formula, pi_formula, max_it, ncomp, tol_ll, pi_link, verbose, initial_weighting, sd_initial = 0.4), progress = TRUE)
grid_output_0.5 = map2(random_seeds_vector, 1:length(random_seeds_vector), ~ run(.x, .y, visible_data, mu_formula, pi_formula, max_it, ncomp, tol_ll, pi_link, verbose, initial_weighting, sd_initial = 0.5), progress = TRUE)



get_like = function(grid_output){
  grid_output$final_like %>% return()
}

summary = map(grid_output, get_like) %>% data.table::rbindlist(.) %>% tibble %>% arrange(desc(likelihood))
print(summary)

summary %>% ggplot() +
  geom_histogram(aes(x = step), binwidth = 1)

summary %>% ggplot() +
  geom_histogram(aes(x = likelihood))


plot_fm(grid_output[[(summary %>% head(1) %>% pull(iter))]]$output, "top iter")
plot_fm(grid_output[[(summary %>% tail(1) %>% pull(iter))]]$output, "bottom iter")


summary = map(grid_output_0.1, get_like) %>% data.table::rbindlist(.) %>% tibble %>% arrange(desc(likelihood))
print(summary)
summary = map(grid_output_0.2, get_like) %>% data.table::rbindlist(.) %>% tibble %>% arrange(desc(likelihood))
print(summary)
summary = map(grid_output_0.3, get_like) %>% data.table::rbindlist(.) %>% tibble %>% arrange(desc(likelihood))
print(summary)
summary = map(grid_output_0.4, get_like) %>% data.table::rbindlist(.) %>% tibble %>% arrange(desc(likelihood))
print(summary)
summary = map(grid_output_0.5, get_like) %>% data.table::rbindlist(.) %>% tibble %>% arrange(desc(likelihood))
print(summary)

get_sigma_init = function(grid_output){
  tibble(c1_scale_init = grid_output$output$possible_data %>% filter(c == "1") %>% pull(sigma_initial) %>% unique,
         c2_scale_init = grid_output$output$possible_data %>% filter(c == "2") %>% pull(sigma_initial) %>% unique,
         grid_output$final_like,
         sd_initial = grid_output$output$sd_initial)
}



map(list(grid_output_0.1,
         grid_output_0.2,
         grid_output_0.3,
         grid_output_0.4,
         grid_output_0.5),
    ~map(.x, get_sigma_init) %>%
      data.table::rbindlist(.) %>%
      tibble %>%
      arrange(desc(likelihood))
    ) %>%
data.table::rbindlist(.) %>% tibble %>% arrange(desc(likelihood)) %>%
  ggplot() +
  geom_point(aes(x = c2_scale_init, y = likelihood, color = factor(sd_initial)))

get_sigma_final = function(grid_output){
  tibble(c1_scale_init = grid_output$output$possible_data %>% filter(c == "1") %>% pull(sigma_initial) %>% unique,
         c2_scale_init = grid_output$output$possible_data %>% filter(c == "2") %>% pull(sigma_initial) %>% unique,
         c1_scale_final = grid_output$output$possible_data %>% filter(c == "1") %>% pull(`sd[Y|t,c]`) %>% unique,
         c2_scale_final = grid_output$output$possible_data %>% filter(c == "2") %>% pull(`sd[Y|t,c]`) %>% unique,
         grid_output$final_like,
         sd_initial = grid_output$output$sd_initial) %>% return()
}


map(list(grid_output_0.1,
         grid_output_0.2,
         grid_output_0.3,
         grid_output_0.4,
         grid_output_0.5),
    ~map(.x, get_sigma_final) %>%
      data.table::rbindlist(.) %>%
      tibble %>%
      arrange(desc(likelihood))
) %>%
  data.table::rbindlist(.) %>% tibble %>% arrange(desc(likelihood)) %>%
  ggplot() +
  geom_point(aes(x = likelihood, y = c1_scale_final, color = factor(sd_initial)))
