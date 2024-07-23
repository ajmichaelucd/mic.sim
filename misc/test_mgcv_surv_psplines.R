low = 0
high = 4
n = 50
t = runif(n, 0, 10)
sigma = rnorm(n, 0, 0.7)
observed_value = (((t-5)/4) ^ 2) + sigma

sim_data = tibble(t, observed_value, low_cons = low, high_cons = high, weights = runif(50, 0.5, 1),)
sim_data %>%
  ggplot() +
  geom_point(aes(x = t, y = observed_value))
df = censor_values(sim_data)
surv_fit = df %>%
  survival::survreg(Surv(time = left_bound,
                         time2 = right_bound,
                         type = "interval2") ~ pspline(t, df = 2.9), weights = weights, data = .,
                    dist = "gaussian")
mgcv_fit = df %>% modify_bounds.mgcv() %>%
  gam(yi ~ s(t, bs = "cr"), family= mgcv::cnorm(link = "identity"), weights = weights, data = .)
sim_data %>%
  ggplot() +
  geom_point(aes(x = t, y = observed_value)) +
  geom_function(fun = function(t){predict(mgcv_fit, newdata = data.frame(t=t))}, color = "orange") +
  geom_function(fun = function(t){predict(surv_fit, newdata = data.frame(t=t))}, color = "blue") +
  geom_hline(yintercept = 0, color = "turquoise", linetype = "dashed") +
  geom_hline(yintercept = 4, color = "turquoise", linetype = "dashed")

mgcv_fit %>% summary()
mgcv_fit$family$getTheta(TRUE)
surv_fit$scale
mgcv_fit



