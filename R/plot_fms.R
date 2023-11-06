#' Title
#'
#' @param output
#' @param title
#'
#' @return
#' @export
#'
#' @examples
plot_fms = function(output, title, cens_dir, add_log_reg = FALSE, s_breakpoint = NULL, r_breakpoint = NULL){

  df = output$possible_data %>% mutate(cens =
                                         case_when(
                                           left_bound == -Inf ~ "lc",
                                           right_bound == Inf ~ "rc",
                                           TRUE ~ "int"
                                         ),
                                       mid =
                                         case_when(
                                           left_bound == -Inf ~ right_bound - 0.5,
                                           right_bound == Inf ~ left_bound + 0.5,
                                           TRUE ~ (left_bound + right_bound) / 2
                                         ))



  if(nrow(df %>% filter(left_bound == -Inf)) > 0){
    plot_min_1 <- (df %>% filter(left_bound == -Inf) %>% pull(right_bound) %>% min(., na.rm = TRUE)) - 1
  }else{
    plot_min_1 <- (df %>% pull(left_bound) %>% min(., na.rm = TRUE)) - 1
  }

  plot_min_2 <- sim_pi_survreg_boot(df, fit = output$newmodel, alpha = 0.05, nSims = 10000) %>% pull(lwr) %>% min(., na.rm = TRUE)


  plot_min = min(plot_min_1, plot_min_2, na.rm = TRUE)


  if(nrow(df %>% filter(right_bound == Inf)) > 0){
    plot_max_1 <- (df %>% filter(right_bound == Inf) %>% pull(left_bound) %>% max) + 1
  }else{
    plot_max_1 <- (df %>% pull(right_bound) %>% max) + 1
  }


  plot_max_2 <- sim_pi_survreg_boot(df, fit = output$newmodel, alpha = 0.05, nSims = 10000) %>% pull(upr) %>% max

  plot_max = max(plot_max_1, plot_max_2)

  ci_data <- tibble(t = rep(seq(0, max(output$possible_data$t), len = 300), 2)) %>%
    mutate(
      c1pred = predict(output$newmodel, tibble(t), se = T)$fit,
      c1pred_se = predict(output$newmodel, tibble(t), se = T)$se.fit,
      c1pred_lb = c1pred - 1.96 * c1pred_se,
      c1pred_ub = c1pred + 1.96 * c1pred_se
    )

  mu.se.brd.safety <- function(t, z){predict(output$newmodel, data.frame(t = t)) + z * predict(output$newmodel, data.frame(t = t), se = TRUE)$se.fit}

  if(cens_dir == "LC"){color_comp = "Component 2 Mu"}else if(cens_dir == "RC"){color_comp = "Component 1 Mu"}else{errorCondition("Pick LC or RC for cens_dir")}

  mean <- df %>% ggplot() +
    geom_point(aes(x = 1, y = 1, color = "Component 1 Mu", fill = "Component 2 Mu"), alpha = 0) +
    geom_point(aes(x = 1, y = 1, color = "Component 2 Mu", fill = "Component 2 Mu"), alpha = 0) +
    geom_function(fun = function(t){predict(output$newmodel, newdata = data.frame(t = t))}, aes(color = color_comp, linetype = "Fitted Model")) +
    geom_ribbon(aes(ymin = c1pred_lb, ymax = c1pred_ub, x = t, fill = color_comp), data = ci_data, alpha = 0.25) +
    geom_ribbon(aes(ymin = lwr, ymax = upr, x = t, fill = color_comp), data = sim_pi_survreg_boot(df, fit = output$newmodel, alpha = 0.05, nSims = 10000), alpha = 0.15) +
    ggnewscale::new_scale_color() +
    scale_color_gradient2(low = "red", high = "blue", mid = "green", midpoint = 0.5) +
    #geom_point(aes(x = t, y = mid, color = `P(C=c|y,t)`), data = df %>% filter(c == "2"), alpha = 0) +
    geom_segment(aes(x = t, xend = t, y = left_bound, yend = right_bound, color = `P(C=c|y,t)`), data = (df %>% filter(cens == "int" & c == "2")), alpha = 0.2) +
    geom_segment(aes(x = t, xend = t, y = right_bound, yend = left_bound, color = `P(C=c|y,t)`), data = (df %>% filter(cens == "lc" & c == "2") %>% mutate(left_bound = plot_min)), arrow = arrow(length = unit(0.03, "npc")), alpha = 0.2) +
    geom_segment(aes(x = t, xend = t, y = left_bound, yend = right_bound, color = `P(C=c|y,t)`), data = (df %>% filter(cens == "rc" & c == "2") %>% mutate(right_bound = plot_max)), arrow = arrow(length = unit(0.03, "npc")), alpha = 0.2) +
    geom_point(aes(x = t, y = left_bound, color = `P(C=c|y,t)`), data = df %>% filter(left_bound != -Inf & c == "2"), alpha = 0.2) +
    geom_point(aes(x = t, y = right_bound, color = `P(C=c|y,t)`), data = df %>% filter(right_bound != Inf & c == "2"), alpha = 0.2) +
    #ylim(plot_min - 0.5, plot_max + 0.5) +
    ggtitle(title) +
    xlab("Time") +
    ylab("MIC") +
    ylim(plot_min, plot_max)
  #  geom_function(fun = function(t){mu.se.brd(t, c = 2, z = 1.96)}, aes(color = "Component 2 Mu", linetype = "Fitted Model SE"), size = 0.6, alpha = 0.6) +
  #  geom_function(fun = function(t){mu.se.brd(t, c = 2, z = -1.96)}, aes(color = "Component 2 Mu", linetype = "Fitted Model SE"), size = 0.6, alpha = 0.6)


  pi <- df %>%
    ggplot() +
    #  geom_point(aes(x = t, y = pi_hat, color = "Fitted Model")) +
    geom_function(
      fun = function(t) {
        predict(output$binom_model, data.frame(t), type = "response")
      },
      aes(color = "Proportion non-WT")
    ) +
    geom_function(
      fun = function(t) {
        (1 - predict(output$binom_model, data.frame(t), type = "response"))
      },
      aes(color = "Proportion WT")
    ) +
    xlim(min(output$possible_data$t), max(output$possible_data$t)) +
    ylim(0, 1)

  if(add_log_reg & !is.null(s_breakpoint) & !is.null(r_breakpoint) & !is.na(s_breakpoint) & !is.na(r_breakpoint)){
    lr_output = log_reg(output$possible_data, data_type = "possible_data", drug = NULL, date_col = "t", date_type = "decimal", first_year = NULL, s_breakpoint = s_breakpoint, r_breakpoint = r_breakpoint)

    pi = pi +
      geom_function(fun = function(t){(1 - predict(lr_output, newdata = data.frame(t = t), type = "response"))}, aes(color = "Susceptible", linetype = "Logistic Regression")) +
      geom_function(fun = function(t){predict(lr_output, newdata = data.frame(t = t), type = "response")}, aes(color = "Resistant", linetype = "Logistic Regression"))
    mean = mean +
      ggnewscale::new_scale_color() +
      geom_hline(aes(yintercept = ((s_breakpoint %>% parse_number() %>% log2) - 1), color = "Susceptible Breakpoint"), alpha = 0.4) +
      geom_hline(aes(yintercept = ((r_breakpoint %>% parse_number() %>% log2) - 1), color = "Resistant Breakpoint"), alpha = 0.4) +
      scale_color_viridis_d(option = "turbo")
  }

  return(mean/pi)

}
