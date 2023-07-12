number_of_batches = 100
#number per batch (e.g. 10)
number_per_batch = 10
#check by putting total number here
number_of_iterations = 1000


#scratch for loading in a plot
iteration = 158



#calculate batch from iteration

file = "~/Desktop/june_2023/run_form2_loess_1_06132023_1.Rdata"

file  <- gen_path_sim(location = location, format = format, array_name = array_name, date = date, i = ceiling(iteration / number_per_batch))
batch_results <- loadRData(file)

j <- iteration - (floor(iteration / number_per_batch) * number_per_batch)

results <- batch_results$model_results[[j]]
settings <- batch_results$settings



results$single_model_output$possible_data %>% filter(comp == c) %>%
  mutate(pi_hat = predict(results$single_model_output$binom_model, data.frame(t = t), type = "response")) %>%
  rowwise %>%
  mutate(pi_dgm = settings$pi(t) %>% pull(2)) %>%
  ungroup %>%
  mutate(false_resid = pi_dgm - pi_hat,
         resid = (c == "2") * 1 - pi_hat ) %>%
  ggplot() +
  #  geom_point(aes(x = t, y = pi_hat, color = "Fitted Model")) +
  geom_function(fun = function(t){predict(results$single_model_output$binom_model, data.frame(t), type = "response")}, aes(color = "Fitted Model")) +
  #   geom_point(aes(x = t, y = pi_dgm, color = "Data Generating Mechanism")) +
  ggplot2::geom_function(fun = function(t){settings$pi(t) %>% pull(2)}, aes(color = "Data Generating Mechanism")) +
  geom_smooth(aes(x = t, y = (c == "2") * 1))






mu.se <- function(t, c, z){predict(results$single_model_output$newmodel[[c]], data.frame(t = t)) + z * predict(results$single_model_output$newmodel[[c]], data.frame(t = t), se = TRUE)$se.fit}


results$single_model_output$possible_data %>% filter(c == "2") %>%
  mutate(mu_dgm = settings$`E[X|T,C]`(t = t, c = c)) %>%
  mutate(mu_hat = case_when(
    c == "1" ~ predict(results$single_model_output$newmodel[[1]], data.frame(t = t)),
    c == "2" ~ predict(results$single_model_output$newmodel[[2]], data.frame(t = t)),
    TRUE ~ NaN
  ),
  mu_hat_se = case_when(
    c == "1" ~ predict(results$single_model_output$newmodel[[1]], data.frame(t = t), se = TRUE)$se.fit,
    c == "2" ~ predict(results$single_model_output$newmodel[[2]], data.frame(t = t), se = TRUE)$se.fit,
    TRUE ~ NaN
  )) %>%
  mutate(resid = observed_value - mu_hat,
         false_resid = mu_dgm - mu_hat) %>%
  mutate(
    predicted_comp = case_when(
      c == "2" & `P(C=c|y,t)` > 0.5 ~ "2",
      c == "2" & `P(C=c|y,t)` < 0.5 ~ "1",
      c == "1" & `P(C=c|y,t)` > 0.5 ~ "1",
      c == "1" & `P(C=c|y,t)` < 0.5 ~ "2",
      TRUE ~ "both"
    ) #rename observed data to underlying values
  ) %>%
  ggplot() +
  #geom_point(aes(x = t, y = mu_dgm, color = c)) + #make as line: geom_function
  geom_function(fun = function(t){settings$`E[X|T,C]`(t, c = 1)}, aes(color = "Component 1 Mu", linetype = "Data Generating Mechanism"), size = 0.9) +
  geom_function(fun = function(t){settings$`E[X|T,C]`(t, c = 2)}, aes(color = "Component 2 Mu", linetype = "Data Generating Mechanism"), size = 0.9) +
  geom_function(fun = function(t){predict(results$single_model_output$newmodel[[1]], data.frame(t = t))}, aes(color = "Component 1 Mu", linetype = "Fitted Model"), size = 0.9) +
  geom_function(fun = function(t){predict(results$single_model_output$newmodel[[2]], data.frame(t = t))}, aes(color = "Component 2 Mu", linetype = "Fitted Model"), size = 0.9) +
  # geom_point(aes(x = t, y = mu_hat, color = c), fill = "black", shape = 21) + #geom_function
   geom_smooth(aes(x = t, y = observed_value), size = 0.3, alpha = 0.4, data = . %>% filter(comp == "1"), se = FALSE, span = 0.5, color = "red") +  ###fix later
   geom_smooth(aes(x = t, y = observed_value), size = 0.3, alpha = 0.4, data = . %>% filter(comp == "2"), se = FALSE, span = 0.5, color = "orange") + ###fix later
  geom_hline(yintercept = settings$low_con %>% unlist) +
  geom_hline(yintercept = settings$high_con %>% unlist) +
  geom_function(fun = function(t){mu.se(t, c = 1, z = 1.96)}, aes(color = "Component 1 Mu", linetype = "Fitted Model SE"), size = 0.6, alpha = 0.6) +
  geom_function(fun = function(t){mu.se(t, c = 1, z = -1.96)}, aes(color = "Component 1 Mu", linetype = "Fitted Model SE"), size = 0.6, alpha = 0.6) +
  geom_function(fun = function(t){mu.se(t, c = 2, z = 1.96)}, aes(color = "Component 2 Mu", linetype = "Fitted Model SE"), size = 0.6, alpha = 0.6) +
  geom_function(fun = function(t){mu.se(t, c = 2, z = -1.96)}, aes(color = "Component 2 Mu", linetype = "Fitted Model SE"), size = 0.6, alpha = 0.6) +
  ggnewscale::new_scale_color() +
  geom_point(aes(x = t, y = observed_value, color = `P(C=c|y,t)`, shape = comp), alpha = 0.3) +
  ggplot2::scale_colour_gradientn(colours = c("purple", "orange"))



results$single_model_output$newmodel[[1]]$scale
results$single_model_output$newmodel[[2]]$scale





results$single_model_output$possible_data %>%

mutate(
  `P(C=c|y,t)2` = case_when(left_bound > median_y & c == "2" ~ ((((left_bound - median_y) / (high_con - median_y)) * 0.5) + 0.5),
                           left_bound > median_y & c == "1" ~ 1 - ((((left_bound - median_y) / (high_con - median_y)) * 0.5) + 0.5),
                           left_bound <= median_y & left_bound != -Inf & c == "2" ~ 1 - ((((median_y - left_bound) / (median_y - low_con + 1)) * 0.5) + 0.5),
                           left_bound <= median_y & left_bound != -Inf & c == "1" ~ ((((median_y - left_bound) / (median_y - low_con + 1)) * 0.5) + 0.5),
                           left_bound == -Inf & c == "2" ~ 0.99,
                           left_bound == -Inf & c == "1" ~ 0.01),
  mid =
    case_when(
      left_bound == -Inf ~ right_bound - 0.5,
      right_bound == Inf ~ left_bound + 0.5,
      TRUE ~ (left_bound + right_bound) / 2
    ),
  rc = ifelse(right_bound == Inf, TRUE, FALSE)
) %>% select(obs_id:`P(C=c|y,t)`, `P(C=c|y,t)2`) %>%

  View














