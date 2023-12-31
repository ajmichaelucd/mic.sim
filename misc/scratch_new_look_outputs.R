df %>% ggplot() +
  #geom_bar(aes(x = mid, fill = cens)) +
  geom_function(fun = function(t){predict(output$mu_model[[1]], newdata = data.frame(t = t))}, aes(color = "Component 1 Mu", linetype = "Fitted Model")) +
  geom_function(fun = function(t){predict(output$mu_model[[2]], newdata = data.frame(t = t))}, aes(color = "Component 2 Mu", linetype = "Fitted Model")) +
  geom_ribbon(aes(ymin = c1pred_lb, ymax = c1pred_ub, x = t, fill = "Component 1 Mu"), data = ci_data, alpha = 0.25) +
  geom_ribbon(aes(ymin = c2pred_lb, ymax = c2pred_ub, x = t, fill = "Component 2 Mu"), data = ci_data, alpha = 0.25) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, x = t, fill = "Component 1 Mu"), data = sim_pi_survreg_boot(df, fit = output$mu_model[[1]], alpha = 0.05, nSims = 10000), alpha = 0.15) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, x = t, fill = "Component 2 Mu"), data = sim_pi_survreg_boot(df, fit = output$mu_model[[2]], alpha = 0.05, nSims = 10000), alpha = 0.15) +
  scale_color_manual(breaks = c("Component 1 Mu", "Component 2 Mu"), values = c("#F8766D", "#00BFC4")) +
  ggnewscale::new_scale_color() +
  scale_colour_gradient2(high = "blue", low = "red", mid = "green", midpoint = 0.5) +
  #geom_point(aes(x = t, y = mid, color = `P(C=c|y,t)`), data = df %>% filter(c == "2"), alpha = 0) +
  geom_segment(aes(x = t, xend = t, y = left_bound, yend = right_bound, color = `P(C=c|y,t)`), data = (df %>% filter(cens == "int" & c == "2")), alpha = 0.3) +
  geom_segment(aes(x = t, xend = t, y = right_bound, yend = left_bound, color = `P(C=c|y,t)`), data = (df %>% filter(cens == "lc" & c == "2") %>% mutate(plot_min)), arrow = arrow(length = unit(0.03, "npc")), alpha = 0.3) +
  geom_segment(aes(x = t, xend = t, y = left_bound, yend = right_bound, color = `P(C=c|y,t)`), data = (df %>% filter(cens == "rc" & c == "2") %>% mutate(plot_max)), arrow = arrow(length = unit(0.03, "npc")), alpha = 0.3) +
  geom_point(aes(x = t, y = left_bound,  color = `P(C=c|y,t)`), data = df %>% filter(left_bound != -Inf & c == "2"), alpha = 0.3) +
  geom_point(aes(x = t, y = right_bound,  color = `P(C=c|y,t)`), data = df %>% filter(right_bound != Inf & c == "2"), alpha = 0.3) +
  #ylim(plot_min - 0.5, plot_max + 0.5) +
  ggtitle(title) +
  xlab("Time") +
  ylab(bquote(log[2]~ MIC)) +
  ylim(plot_min - 1, plot_max + 1) +
  scale_y_continuous(breaks = scales::breaks_extended((plot_max - plot_min)/1.5)) +
  scale_x_continuous(breaks = scales::breaks_extended(6)) +
  theme_minimal() +


#geom_function(fun = function(t){mu.se.brd(t, c = 1, z = 1.96)}, aes(color = "Component 1 Mu", linetype = "Fitted Model SE"), size = 0.6, alpha = 0.6) +
#geom_function(fun = function(t){mu.se.brd(t, c = 1, z = -1.96)}, aes(color = "Component 1 Mu", linetype = "Fitted Model SE"), size = 0.6, alpha = 0.6) +
#geom_function(fun = function(t){mu.se.brd(t, c = 2, z = 1.96)}, aes(color = "Component 2 Mu", linetype = "Fitted Model SE"), size = 0.6, alpha = 0.6) +
#geom_function(fun = function(t){mu.se.brd(t, c = 2, z = -1.96)}, aes(color = "Component 2 Mu", linetype = "Fitted Model SE"), size = 0.6, alpha = 0.6) +


##find sim_pi_survreg_boot in scratch_add_pi_survreg.R

# need to examine the things for sim_pi_survreg_boot, specifically the vcov stuff and if we should let it draw values for all spline terms and then also for the
#way it calculates the sim response
#do we need to account for weighting or anything?

pi <- ggplot() +
  geom_function(fun = function(t){(1 - predict(output$pi_model, newdata = data.frame(t = t), type = "response"))}, aes(color = "Wild Type", linetype = "Fitted Model")) +
  geom_function(fun = function(t){predict(output$pi_model, newdata = data.frame(t = t), type = "response")}, aes(color = "Non-Wild Type", linetype = "Fitted Model")) +
  xlim(0, 16) +
  ylim(0,1)  +
  xlab("Time") + ylab("Proportion")
