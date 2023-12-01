
load("data-raw/example_data")
visible_data = example_data %>% mutate(obs_id = row_number())
output = EM_algorithm_mgcv(visible_data,
                  mu_formula = yi ~ s(t),
                  pi_formula = c == "2" ~ s(t, bp = "ps"),
                  max_it = 21,
                  ncomp = 2,
                  tol_ll = 1e-6,
                  browse_at_end = FALSE,
                  browse_each_step = FALSE,
                  plot_visuals = FALSE,
                  prior_step_plot = FALSE,
                  pause_on_likelihood_drop = FALSE,
                  pi_link = "logit",
                  verbose = 0,
                  model_coefficient_tolerance = 0.00001,
                  maxiter_survreg = 30,
                  initial_weighting = 8)

output$newmodel[[2]]$family$getTheta(TRUE)

plot_likelihood(output$likelihood, format = "tibble")

output_surv = fit_model_pi(visible_data,
             mu_formula = Surv(time = left_bound, time2 = right_bound, type = "interval2") ~
              pspline(t, df = 0, calc = TRUE),
             pi_formula = c == "2" ~ s(t),
             max_it = 1,
             ncomp = 2,
             tol_ll = 1e-6,
             browse_at_end = FALSE,
             browse_each_step = FALSE,
             plot_visuals = FALSE,
             prior_step_plot = FALSE,
             #pause_on_likelihood_drop = FALSE,
             pi_link = "logit",
             verbose = 3,
             model_coefficient_tolerance = 0.00001,
             maxiter_survreg = 30,
             initial_weighting = 8)


#plot_fm(output)
  #need to modify sim_pi_survreg_boot to use plot_fm

plot = function(possible_data, mu_models_new, pi_model_new, mu_models_old, pi_model_old){
  possible_data = possible_data %>% mutate(cens =
                                    case_when(
                                      left_bound == -Inf ~ "lc",
                                      right_bound == Inf ~ "rc",
                                      TRUE ~ "int"
                                    ),
                                  c1_lb = predict(mu_models_new[[1]], tibble(t), se = T)$fit - 1.96 * (predict(mu_models_new[[1]], tibble(t), se = T)$se.fit),
                                  c1_ub = predict(mu_models_new[[1]], tibble(t), se = T)$fit + 1.96 * (predict(mu_models_new[[1]], tibble(t), se = T)$se.fit),
                                  c2_lb = predict(mu_models_new[[2]], tibble(t), se = T)$fit - 1.96 * (predict(mu_models_new[[2]], tibble(t), se = T)$se.fit),
                                  c2_ub = predict(mu_models_new[[2]], tibble(t), se = T)$fit + 1.96 * (predict(mu_models_new[[2]], tibble(t), se = T)$se.fit),
                                  c1_sigma_lb = predict(mu_models_new[[1]], tibble(t), se = T)$fit - 1.96 * mu_models_new[[1]]$family$getTheta(TRUE),
                                  c1_sigma_ub = predict(mu_models_new[[1]], tibble(t), se = T)$fit + 1.96 * mu_models_new[[1]]$family$getTheta(TRUE),
                                  c2_sigma_lb = predict(mu_models_new[[2]], tibble(t), se = T)$fit - 1.96 * mu_models_new[[2]]$family$getTheta(TRUE),
                                  c2_sigma_ub = predict(mu_models_new[[2]], tibble(t), se = T)$fit + 1.96 * mu_models_new[[2]]$family$getTheta(TRUE)
                                  )
  mean = possible_data %>%
    ggplot() +
    geom_function(fun = function(t){predict(mu_models_old[[1]], newdata = data.frame(t = t))}, aes(color = "Component 1 Mu Prior", linetype = "Fitted Model")) +
    geom_function(fun = function(t){predict(mu_models_old[[2]], newdata = data.frame(t = t))}, aes(color = "Component 2 Mu Prior", linetype = "Fitted Model")) +
    geom_ribbon(aes(ymin = c1_lb, ymax = c1_ub, x = t, fill = "Component 1 Mu"), alpha = 0.25) +
    geom_function(fun = function(t){predict(mu_models_new[[1]], newdata = data.frame(t = t))}, aes(color = "Component 1 Mu", linetype = "Fitted Model")) +
    geom_function(fun = function(t){predict(mu_models_new[[2]], newdata = data.frame(t = t))}, aes(color = "Component 2 Mu", linetype = "Fitted Model")) +
    geom_ribbon(aes(ymin = c2_lb, ymax = c2_ub, x = t, fill = "Component 2 Mu"), alpha = 0.25) +
    geom_ribbon(aes(ymin = c1_sigma_lb, ymax = c1_sigma_ub, x = t, fill = "Component 1 Mu"), alpha = 0.1) +
    geom_ribbon(aes(ymin = c2_sigma_lb, ymax = c2_sigma_ub, x = t, fill = "Component 2 Mu"), alpha = 0.1) +
    ggnewscale::new_scale_color() +
    scale_color_gradient2(low = "red", high = "blue", mid = "green", midpoint = 0.5) +
    geom_segment(aes(x = t, xend = t, y = left_bound, yend = right_bound, color = `P(C=c|y,t)`), data = (possible_data %>% filter(cens == "int" & c == "2")), alpha = 0.3) +
    geom_segment(aes(x = t, xend = t, y = right_bound, yend = left_bound, color = `P(C=c|y,t)`), data = (possible_data %>% filter(cens == "lc" & c == "2") %>% mutate(left_bound = -5)), arrow = arrow(length = unit(0.03, "npc")), alpha = 0.3) +
    geom_segment(aes(x = t, xend = t, y = left_bound, yend = right_bound, color = `P(C=c|y,t)`), data = (possible_data %>% filter(cens == "rc" & c == "2") %>% mutate(right_bound = 7)), arrow = arrow(length = unit(0.03, "npc")), alpha = 0.3) +
    geom_point(aes(x = t, y = left_bound,  color = `P(C=c|y,t)`), data = possible_data %>% filter(left_bound != -Inf & c == "2"), alpha = 0.3) +
    geom_point(aes(x = t, y = right_bound,  color = `P(C=c|y,t)`), data = possible_data %>% filter(right_bound != Inf & c == "2"), alpha = 0.3) +
    #ggtitle(paste0("Iteration ", i, ": Ribbons are the CI for the mean estimates")) +
    xlab("Time") +
    ylab("MIC") +
    ylim(-11, 15)

  pi = ggplot() +
    geom_function(fun = function(t){(1 - predict(pi_model_old, newdata = data.frame(t = t), type = "response"))}, aes(color = "Component 1 Prior")) +
    geom_function(fun = function(t){predict(pi_model_old, newdata = data.frame(t = t), type = "response")}, aes(color = "Component 2 Prior")) +
    geom_function(fun = function(t){(1 - predict(pi_model_new, newdata = data.frame(t = t), type = "response"))}, aes(color = "Component 1")) +
    geom_function(fun = function(t){predict(pi_model_new, newdata = data.frame(t = t), type = "response")}, aes(color = "Component 2")) +
    xlim(0, 16) +
    ylim(0,1)

  return(mean/pi)
}


plot(output$possible_data, output$newmodel, output$binom_model, output$prior_step_models$mu_models, output$prior_step_models$pi_model)
#output$newmodel[[2]]$family$getTheta(TRUE)

plot_fm(output_surv, title = "surv")
