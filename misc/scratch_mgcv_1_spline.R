
n=300
`E[X|T]` = function(t){ -2.0 + (1.5 * t) - (0.4 * (t^2)) + (0.018 * (t^3)) + (0.0009 * (t^5))}
t_dist = function(n){runif(n, min = 0, max = 5)}
attr(t_dist, "min") = 0
attr(t_dist, "max") = 5
sd = 0.8
low_con = -3
high_con = 3

ggplot() +
  geom_function(fun = `E[X|T]`) + xlim(attr(t_dist, "min") ,attr(t_dist, "max") ) + ylim(-3,3)

df = tibble(t = t_dist(n)) %>%
  mutate(obs_id = row_number()) %>%
  select(obs_id, everything()) %>%
  mutate(.by = obs_id,
         x = `E[X|T]`(t),
         epsilon = rnorm(1, 0, sd),
         true_value = x + epsilon,
         left_bound = case_when(
           true_value < low_con ~ -Inf,
           true_value > high_con ~ high_con,
           TRUE ~ floor(true_value)
         ),
         right_bound = case_when(
           true_value > high_con ~ Inf,
           true_value < low_con ~ low_con,
           TRUE ~ ceiling(true_value)
         ),
         left_bound_mgcv =
           case_when(
             left_bound == -Inf ~ right_bound,
             TRUE ~ left_bound
           ),
         right_bound_mgcv =
           case_when(
             left_bound == -Inf ~ -Inf,
             TRUE ~ right_bound
           ))

df %>%
  ggplot() +
  geom_point(aes(x = t, y = true_value)) +
  geom_function(fun = `E[X|T]`) + xlim(attr(t_dist, "min") ,attr(t_dist, "max") ) + ylim(-3,3)

df %>% ggplot() +
  geom_segment(aes(x = t, xend = t, y = left_bound, yend = right_bound), data = (. %>% filter(left_bound != -Inf & right_bound != Inf)), alpha = 0.3) +
  geom_segment(aes(x = t, xend = t, y = right_bound, yend = left_bound), data = (. %>% filter(left_bound == -Inf) %>% mutate(left_bound = low_con - 2)), arrow = arrow(length = unit(0.03, "npc")), alpha = 0.3) +
  geom_segment(aes(x = t, xend = t, y = left_bound, yend = right_bound), data = (. %>% filter(right_bound == Inf) %>% mutate(right_bound = high_con + 2)), arrow = arrow(length = unit(0.03, "npc")), alpha = 0.3) +
  geom_point(aes(x = t, y = left_bound), data = . %>% filter(left_bound != -Inf), alpha = 0.3) +
  geom_point(aes(x = t, y = right_bound), data = . %>% filter(right_bound != Inf), alpha = 0.3) +
  geom_function(fun = `E[X|T]`) + xlim(attr(t_dist, "min") ,attr(t_dist, "max") ) + ylim(-3,3)

df$yi = cbind(df$left_bound_mgcv, df$right_bound_mgcv)
ml_model = mgcv::gam(yi ~ s(t), family= mgcv::cnorm(link = "identity"), data=df, method = "P-ML")
reml_model = mgcv::gam(yi ~ s(t), family= mgcv::cnorm(link = "identity"), data=df, method = "P-REML")

df %>%
  mutate(
    ml_lb = predict(model, tibble(t), se = T)$fit - 1.96 * ml_model$family$getTheta(TRUE),
    ml_ub = predict(model, tibble(t), se = T)$fit + 1.96 * ml_model$family$getTheta(TRUE),
    reml_lb = predict(model, tibble(t), se = T)$fit - 1.96 * reml_model$family$getTheta(TRUE),
    reml_ub = predict(model, tibble(t), se = T)$fit + 1.96 * reml_model$family$getTheta(TRUE)
  ) %>%
  ggplot() +
  geom_segment(aes(x = t, xend = t, y = left_bound, yend = right_bound), data = (. %>% filter(left_bound != -Inf & right_bound != Inf)), alpha = 0.3) +
  geom_segment(aes(x = t, xend = t, y = right_bound, yend = left_bound), data = (. %>% filter(left_bound == -Inf) %>% mutate(left_bound = low_con - 2)), arrow = arrow(length = unit(0.03, "npc")), alpha = 0.3) +
  geom_segment(aes(x = t, xend = t, y = left_bound, yend = right_bound), data = (. %>% filter(right_bound == Inf) %>% mutate(right_bound = high_con + 2)), arrow = arrow(length = unit(0.03, "npc")), alpha = 0.3) +
  geom_point(aes(x = t, y = left_bound), data = . %>% filter(left_bound != -Inf), alpha = 0.3) +
  geom_point(aes(x = t, y = right_bound), data = . %>% filter(right_bound != Inf), alpha = 0.3) +
  geom_function(fun = function(t){predict(ml_model, newdata = data.frame(t =t))}, aes(color = "ML")) +
  geom_function(fun = function(t){predict(reml_model, newdata = data.frame(t =t))},  aes(color = "REML")) +
  geom_ribbon(aes(ymin = ml_lb, ymax = ml_ub, x = t, fill = "ML Scale * 1.96"), alpha = 0.1) +
  geom_ribbon(aes(ymin = reml_lb, ymax = reml_ub, x = t, fill = "REML Scale * 1.96"), alpha = 0.1)


