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
sd_initial = 0.2



run = function(seed, iter, visible_data, mu_formula, pi_formula, max_it, ncomp, tol_ll, pi_linl, verbose, initial_weighting){

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
                      randomize = "all"
    )

  if(length(single_model_output) > 1){
  final_like = single_model_output$likelihood %>% select(step, likelihood) %>% tail(1) %>% mutate(iter = iter, seed = seed, converge = single_model_output$converge)
  }else{
    final_like = tibble(step = NA_integer_, likelihood = NA_integer_, iter = iter, seed = seed, converge = "Error")
  }
  list(output = single_model_output, seed = seed, iter = iter, final_like = final_like)

}

grid_output = map2(random_seeds_vector, 1:length(random_seeds_vector), ~ run(.x, .y, visible_data, mu_formula, pi_formula, max_it, ncomp, tol_ll, pi_linl, verbose, initial_weighting), progress = TRUE)

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

?plot_fm






random_lines = function(visible_data, ncomp, sd_parameter = 0.2){

  if(ncomp != 2){
    errorCondition("This initial weighting scheme is appropriate for 2 component models")
  }

  visible_data <- visible_data %>% mutate(cens = case_when(
    left_bound == -Inf | right_bound == low_con ~ "lc",
    right_bound == Inf |
      left_bound == high_con ~ "rc",
    TRUE ~ "int"
  ))
  n_obs <- nrow(visible_data)

  full_set = tibble(
    cens = c("rc", "lc", "int")
  )

  cens_counts <-
    visible_data %>%
    summarize(.by = cens,
              n = n()
    ) %>% right_join(., full_set, by = join_by(cens)) %>% mutate(n = case_when(
      is.na(n) ~ 0,
      TRUE ~ n
    )) %>% pivot_wider(
      names_from = cens, values_from = n
    )

  lc <- cens_counts %>% pull(lc)
  int <- cens_counts %>% pull(int)
  rc <- cens_counts %>% pull(rc)
  `P(1)` <- (lc + (0.5 * int) + 1) / (n_obs + 2)
  `P(2)` <- (rc + (0.5 * int) + 1) / (n_obs + 2)

  change_pi = rnorm(1, mean = 0, sd = 0.05)

  if((`P(1)` + change_pi) > 1){`P(1)` = 1 - 0.01
  }else if((`P(1)` + change_pi) < 0){`P(1)` = 0.01
  }else{`P(1)` = `P(1)` + change_pi}

  `P(2)` = 1 - `P(1)`

  mu_space = (high_con - low_con) / 6

  for(i in 1:7){
    change_mu_1 = rnorm(1, 0, 1) * mu_space
    change_mu_2 = rnorm(1, 0, 1) * mu_space
    if((change_mu_1 - change_mu_2) < (high_con - low_con)){
      break
    }
  }

  visible_data %>%
    reframe(.by = everything(),
            c = as.character(1:2)
    ) %>%
    mutate(
      `E[Y|t,c]` = case_when(c == "1" ~ low_con + change_mu_1,
                             c == "2" ~ high_con + change_mu_2,
                             TRUE ~ NaN),
      `sd[Y|t,c]` = case_when(c == "1" ~ sd_parameter * (high_con - low_con),
                              c == "2" ~  sd_parameter * (high_con - low_con),
                              TRUE ~ NaN),
      `P(Y|t,c)` = case_when(
        left_bound == right_bound ~ dnorm(x = left_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`),
        left_bound <= `E[Y|t,c]` ~ pnorm(right_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`) -
          pnorm(left_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`),
        TRUE ~ pnorm(left_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`, lower.tail = FALSE) -
          pnorm(right_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`, lower.tail = FALSE)
      )
    ) %>%
    mutate(`P(C=c|t)` = case_when(
      c == "2" ~ `P(2)`,
      c == "1" ~ `P(1)`
    ),
    `P(c,y|t)` = `P(C=c|t)` * `P(Y|t,c)`) %>%
    mutate(.by = obs_id,
           `P(Y=y|t)` = sum(`P(c,y|t)`)) %>%
    mutate(
      `P(C=c|y,t)` = `P(c,y|t)` / `P(Y=y|t)`) %>%
    return()
}


