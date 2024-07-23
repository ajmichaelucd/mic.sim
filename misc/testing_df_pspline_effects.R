library(purrr)
library(data.table)

low = 0
high = 4
n = 50
t_max = 10
sigma_width = 0.7
#t = runif(n, 0, 10)
#sigma = rnorm(n, 0, 0.7)
#observed_value = (((t-5)/4) ^ 2) + sigma

check_1_combo = function(a,b, df){
  m = df %>%
    survival::survreg(Surv(time = left_bound,
                           time2 = right_bound,
                           type = "interval2") ~ pspline(t, df = a, nterms = b), weights = weights, data = .,
                      dist = "gaussian")
  return(
    list(
      a,
      b,
      m$df[2],
      m$scale,
      m$penalty[2],
      mean(df$observed_value)
    )
  )
}
check_1_combo_safely = safely(check_1_combo)

compress = function(input){
  tibble(df = input$result[[1]], nterms = input$result[[2]], df_out = input$result[[3]], scale = input$result[[4]], penalty = input$result[[5]], mean = input$result[[6]]) %>% return()
}

run_1_test_iter = function(i, low, high, n, t_max, sigma_width, observed_value){
set.seed(i)

t = runif(n, 0, t_max)
sigma = rnorm(n, 0, sigma_width)
observed_value = (((t-5)/4) ^ 2) + sigma

sim_data = tibble(t, observed_value, low_cons = low, high_cons = high, weights = runif(50, 0.5, 1))
sim_data %>%
  ggplot() +
  geom_point(aes(x = t, y = observed_value))
df = censor_values(sim_data)
d = expand.grid(a = c(1:20), b = c(1:20)) %>% tibble()
out = map2(d$a, d$b, ~check_1_combo_safely(.x, .y, df))


results = out %>% map(., ~compress(.x)) %>% rbindlist() %>% tibble() %>% mutate(iter = i) %>% return()}

a = run_1_test_iter(i = 2, low, high, n, t_max, sigma_width, observed_value)

test_output = map(1:50, ~run_1_test_iter(i = .x, low, high, n, t_max, sigma_width, observed_value))

test_output[[3]]

test_output %>%
  rbindlist() %>%
  tibble() %>%
  summarise(.by = c(df, nterms),
                          df_out_avg = mean(df_out),
                          df_out_se = sd(df_out),
                          scale_avg = mean(scale),
                          scale_se = sd(scale),
                          penalty_avg = mean(penalty),
                          penalty_se = sd(penalty),
                          mean_avg = mean(mean),
                          mean_se = sd(mean)
) %>% View()


test_output %>%
  rbindlist() %>%
  tibble() %>% summarise(.by = c(df, nterms), df_out = mean(df_out),
                         df_out_se = sd(df_out),sd = sd(mean))



sim_data = tibble(t, observed_value, low_cons = low, high_cons = high, weights = runif(50, 0.5, 1),)
sim_data %>%
  ggplot() +
  geom_point(aes(x = t, y = observed_value))
df = censor_values(sim_data)
d = expand.grid(a = c(1:20), b = c(1:20)) %>% tibble()
out = map2(d$a, d$b, ~check_1_combo_safely(.x, .y))


results = out %>% map(., ~compress(.x)) %>% rbindlist() %>% tibble()
results
surv_fit %>% summary()
mgcv_fit$family$getTheta()
