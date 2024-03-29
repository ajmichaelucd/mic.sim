#' Title
#'
#' @param output
#' @param title
#' @param add_log_reg
#' @param s_breakpoint
#' @param r_breakpoint
#'
#'
#' @import ggplot2
#' @import ggnewscale
#'
#' @return
#' @export
#'
#' @examples
plot_fm <- function(output, title, add_log_reg = FALSE, s_breakpoint = NA, r_breakpoint = NA, use_prior_step = FALSE, range_zoom = FALSE){

  if(!is.null(output$prior_step_models) & use_prior_step){
    output$mu_model = output$prior_step_models$mu_models
  }

    if(output$ncomp == "2"){
      results <- tibble(c = 1:2, dnc = purrr::map_lgl(output$mu_model, ~check_comp_conv(.x)))
      if(nrow(results %>% filter(dnc)) > 0){
        fitted_comp = output$mu_model[[results %>% filter(!dnc) %>% pull(c)]]
        ncomp = 1
      } else{
        ncomp = 2
        fitted_comp = NULL
      }
    }else{
      fitted_comp = output$mu_model
      ncomp = 1
    }

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


plot_min <- plot_bounds(df, "min", ncomp, range_zoom, output, fitted_comp)
plot_max <- plot_bounds(df, "max", ncomp, range_zoom, output, fitted_comp)



  #ciTools::add_pi(df, output$mu_model[[1]], alpha = 0.05, names = c("lwr", "upr"))
  #doesn't work with gaussian dist


  mu.se.brd <- function(t, c, z){predict(output$mu_model[[c]], data.frame(t = t)) + z * predict(output$mu_model[[c]], data.frame(t = t), se = TRUE)$se.fit}
  mu.se.brd.fms <- function(t, z){predict(fitted_comp, data.frame(t = t)) + z * predict(fitted_comp, data.frame(t = t), se = TRUE)$se.fit}

  if(ncomp == 2){

    output$mu_model[[1]]$scale %>% print
    output$mu_model[[2]]$scale %>% print

    ci_data <- tibble(t = rep(seq(0, max(output$possible_data$t), len = 300), 2)) %>%
      mutate(
        c1pred = predict(output$mu_model[[1]], tibble(t), se = T)$fit,
        c1pred_se = predict(output$mu_model[[1]], tibble(t), se = T)$se.fit,
        c1pred_lb = c1pred - 1.96 * c1pred_se,
        c1pred_ub = c1pred + 1.96 * c1pred_se,
        c2pred = predict(output$mu_model[[2]], tibble(t), se = T)$fit,
        c2pred_se = predict(output$mu_model[[2]], tibble(t), se = T)$se.fit,
        c2pred_lb = c2pred - 1.96 * c2pred_se,
        c2pred_ub = c2pred + 1.96 * c2pred_se,
      )

    mean <- df %>% ggplot() +
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
      xlab("Time (Years Since 2007)") +
      ylab(bquote(log[2]~ MIC)) +
      ylim(plot_min - 1, plot_max + 1) +
      scale_y_continuous(breaks = scales::breaks_extended((plot_max - plot_min)/1.5)) +
      scale_x_continuous(breaks = scales::breaks_extended(6)) +
      theme_minimal()
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
    if(add_log_reg && !is.null(s_breakpoint) & !is.null(r_breakpoint)){
      if(!is.na(s_breakpoint) & !is.na(r_breakpoint)){
      lr_output = log_reg(output$possible_data, data_type = "possible_data", drug = NULL, date_col = "t", date_type = "decimal", first_year = NULL, s_breakpoint = s_breakpoint, r_breakpoint = r_breakpoint)

      pi = pi +
        geom_function(fun = function(t){(1 - predict(lr_output, newdata = data.frame(t = t), type = "response"))}, aes(color = "Susceptible", linetype = "Logistic Regression")) +
        geom_function(fun = function(t){predict(lr_output, newdata = data.frame(t = t), type = "response")}, aes(color = "Resistant", linetype = "Logistic Regression")) +
        scale_color_manual(breaks = c("Wild Type", "Non-Wild Type", "Susceptible", "Resistant"), values = c("#F8766D", "#00BFC4", "#7CAE00", "#C77CFF"))

      mean = mean +
        ggnewscale::new_scale_color() +
        geom_hline(aes(yintercept = ((s_breakpoint %>% parse_number() %>% log2) - 1), color = "Susceptible Breakpoint", linetype = "Breakpoint"), alpha = 0.4) +
        geom_hline(aes(yintercept = ((r_breakpoint %>% parse_number() %>% log2) - 1), color = "Resistant Breakpoint", linetype =  "Breakpoint"), alpha = 0.4) +
        scale_color_manual(breaks = c("Susceptible Breakpoint", "Resistant Breakpoint"), values = c("#7CAE00", "#C77CFF")) +
        scale_linetype_manual(breaks=c("Fitted Model","Breakpoint"), values=c(1,5))
    }}
    return(mean/pi)

  }else{
    if(output$ncomp == 1){
      ci_data <- tibble(t = rep(seq(0, max(output$possible_data$t), len = 300), 2)) %>%
        mutate(
          c1pred = predict(fitted_comp, tibble(t), se = T)$fit,
          c1pred_se = predict(fitted_comp, tibble(t), se = T)$se.fit,
          c1pred_lb = c1pred - 1.96 * c1pred_se,
          c1pred_ub = c1pred + 1.96 * c1pred_se
        )

      #fitted_comp$scale %>% print()

      mean <- df %>% ggplot() +
        #geom_bar(aes(x = mid, fill = cens)) +
        geom_function(fun = function(t){predict(fitted_comp, newdata = data.frame(t = t))}, aes(color = "Component Mu", linetype = "Fitted Model")) +
        geom_function(fun = function(t){mu.se.brd.fms(t, z = 1.96)}, aes(color = "Component Mu", linetype = "Fitted Model SE"), size = 0.6, alpha = 0.6) +
        geom_function(fun = function(t){mu.se.brd.fms(t, z = -1.96)}, aes(color = "Component Mu", linetype = "Fitted Model SE"), size = 0.6, alpha = 0.6) +
        geom_ribbon(aes(ymin = c1pred_lb, ymax = c1pred_ub, x = t, fill = "Component Mu"), data = ci_data, alpha = 0.2) +
        geom_ribbon(aes(ymin = lwr, ymax = upr, x = t, fill = "Component Mu"), data = sim_pi_survreg_boot(df, fit = fitted_comp, alpha = 0.05, nSims = 10000), alpha = 0.15) +
        ggnewscale::new_scale_color() +
        #geom_point(aes(x = t, y = mid, color = `P(C=c|y,t)`), data = df %>% filter(c == "2"), alpha = 0) +
        geom_segment(aes(x = t, xend = t, y = left_bound, yend = right_bound, color = cens), data = (df %>% filter(cens == "int")), alpha = 0.3) +
        geom_segment(aes(x = t, xend = t, y = right_bound, yend = left_bound, color = cens), data = (df %>% filter(cens == "lc") %>% mutate(left_bound = plot_min)), arrow = arrow(length = unit(0.03, "npc")), alpha = 0.3) +
        geom_segment(aes(x = t, xend = t, y = left_bound, yend = right_bound, color = cens), data = (df %>% filter(cens == "rc") %>% mutate(right_bound = plot_max)), arrow = arrow(length = unit(0.03, "npc")), alpha = 0.3) +
        geom_point(aes(x = t, y = left_bound,  color = cens), data = df %>% filter(left_bound != -Inf), alpha = 0.3) +
        geom_point(aes(x = t, y = right_bound,  color = cens), data = df %>% filter(right_bound != Inf), alpha = 0.3) +
        #scale_colour_gradientn(colours = c("purple", "orange")) +
        #ylim(plot_min - 0.5, plot_max + 0.5) +
        ggtitle(title) +
        xlab("Time (Years Since 2007)") +
        ylab(bquote(log[2]~ MIC)) +
        ylim(plot_min - 1, plot_max + 1) +
        scale_y_continuous(breaks = scales::breaks_extended((plot_max - plot_min)/1.5)) +
        scale_x_continuous(breaks = scales::breaks_extended(6)) +
        theme_minimal()

      return(mean)

    }else{
      ci_data <- tibble(t = rep(seq(0, max(output$possible_data$t), len = 300), 2)) %>%
        mutate(
          c1pred = predict(fitted_comp, tibble(t), se = T)$fit,
          c1pred_se = predict(fitted_comp, tibble(t), se = T)$se.fit,
          c1pred_lb = c1pred - 1.96 * c1pred_se,
          c1pred_ub = c1pred + 1.96 * c1pred_se
        )
      if((results %>% filter(!dnc) %>% pull(c)) == 1){
      corresponding_color = "#F8766D"}else{ #comp1
      corresponding_color = "#00BFC4"}#comp2


      mean <- df %>% ggplot() +
        #geom_bar(aes(x = mid, fill = cens)) +
        geom_function(fun = function(t){predict(fitted_comp, newdata = data.frame(t = t))}, aes(color = "Component Mu", linetype = "Fitted Model")) +
        geom_function(fun = function(t){mu.se.brd.fms(t, z = 1.96)}, aes(color = "Component Mu", linetype = "Fitted Model SE"), size = 0.6, alpha = 0.6) +
        geom_function(fun = function(t){mu.se.brd.fms(t, z = -1.96)}, aes(color = "Component Mu", linetype = "Fitted Model SE"), size = 0.6, alpha = 0.6) +
        geom_ribbon(aes(ymin = c1pred_lb, ymax = c1pred_ub, x = t, fill = "Component Mu"), data = ci_data, alpha = 0.2) +
        geom_ribbon(aes(ymin = lwr, ymax = upr, x = t, fill = "Component Mu"), data = sim_pi_survreg_boot(df, fit = fitted_comp, alpha = 0.05, nSims = 10000), alpha = 0.15) +
        scale_color_manual(breaks = c("Component Mu"), values = c(corresponding_color)) +
        scale_fill_manual(breaks = c("Component Mu"), values = c(corresponding_color)) +
        ggnewscale::new_scale_color() +
        scale_colour_gradient2(high = "blue", low = "red", mid = "green", midpoint = 0.5) +
        #geom_point(aes(x = t, y = mid, color = `P(C=c|y,t)`), data = df %>% filter(c == "2"), alpha = 0) +
        geom_segment(aes(x = t, xend = t, y = left_bound, yend = right_bound, color = `P(C=c|y,t)`), data = (df %>% filter(cens == "int" & c == "2")), alpha = 0.2) +
        geom_segment(aes(x = t, xend = t, y = right_bound, yend = left_bound, color = `P(C=c|y,t)`), data = (df %>% filter(cens == "lc" & c == "2") %>% mutate(left_bound = plot_min)), arrow = arrow(length = unit(0.03, "npc")), alpha = 0.2) +
        geom_segment(aes(x = t, xend = t, y = left_bound, yend = right_bound, color = `P(C=c|y,t)`), data = (df %>% filter(cens == "rc"& c == "2") %>% mutate(right_bound = plot_max)), arrow = arrow(length = unit(0.03, "npc")), alpha = 0.2) +
        geom_point(aes(x = t, y = left_bound,  color = `P(C=c|y,t)`), data = df %>% filter(left_bound != -Inf & c == "2"), alpha = 0.2) +
        geom_point(aes(x = t, y = right_bound,  color = `P(C=c|y,t)`), data = df %>% filter(right_bound != Inf & c == "2"), alpha = 0.2) +
        #scale_colour_gradientn(colours = c("purple", "orange")) +
        #ylim(plot_min - 0.5, plot_max + 0.5) +
        ggtitle(title) +
        xlab("Time (Years Since 2007)") +
        ylab(bquote(log[2]~ MIC)) +
        ylim(plot_min - 1, plot_max + 1) +
        scale_y_continuous(breaks = scales::breaks_extended((plot_max - plot_min)/1.5)) +
        scale_x_continuous(breaks = scales::breaks_extended(6)) +
        theme_minimal()

      pi <- ggplot() +
        geom_function(fun = function(t){(1 - predict(output$pi_model, newdata = data.frame(t = t), type = "response"))}, aes(color = "Wild Type", linetype = "Fitted Model")) +
        geom_function(fun = function(t){predict(output$pi_model, newdata = data.frame(t = t), type = "response")}, aes(color = "Non-Wild Type", linetype = "Fitted Model")) +
        xlim(0, 16) +
        ylim(0,1)  +
        xlab("Time") + ylab("Proportion")


      if(add_log_reg && !is.null(s_breakpoint) & !is.null(r_breakpoint)){
        if(!is.na(s_breakpoint) & !is.na(r_breakpoint)){
          lr_output = log_reg(output$possible_data, data_type = "possible_data", drug = NULL, date_col = "t", date_type = "decimal", first_year = NULL, s_breakpoint = s_breakpoint, r_breakpoint = r_breakpoint)

pi = pi +
  geom_function(fun = function(t){(1 - predict(lr_output, newdata = data.frame(t = t), type = "response"))}, aes(color = "Susceptible", linetype = "Logistic Regression")) +
  geom_function(fun = function(t){predict(lr_output, newdata = data.frame(t = t), type = "response")}, aes(color = "Resistant", linetype = "Logistic Regression")) +
  scale_color_manual(breaks = c("Wild Type", "Non-Wild Type", "Susceptible", "Resistant"), values = c("#F8766D", "#00BFC4", "#7CAE00", "#C77CFF"))

mean = mean +
  ggnewscale::new_scale_color() +
  geom_hline(aes(yintercept = ((s_breakpoint %>% parse_number() %>% log2) - 1), color = "Susceptible Breakpoint", linetype = "Breakpoint"), alpha = 0.4) +
  geom_hline(aes(yintercept = ((r_breakpoint %>% parse_number() %>% log2) - 1), color = "Resistant Breakpoint", linetype =  "Breakpoint"), alpha = 0.4) +
  scale_color_manual(breaks = c("Susceptible Breakpoint", "Resistant Breakpoint"), values = c("#7CAE00", "#C77CFF")) +
  scale_linetype_manual(breaks=c("Fitted Model","Breakpoint", "Fitted Model SE"), values=c(1,5,3))
        }
      }
      return(mean/pi)
    }
  }


  ##maxing out iterations fails to generate a `converge` object!!!!!!!!!!!!!!

}

plot_bounds = function(df, side, ncomp, range_zoom = FALSE, output, fitted_comp = NULL){
  if(side == "min"){
    if(nrow(df %>% filter(left_bound == -Inf)) > 0){
      plot_min_1 <- (df %>% filter(left_bound == -Inf) %>% pull(right_bound) %>% min(., na.rm = TRUE)) - 2.5
    }else{
      plot_min_1 <- (df %>% pull(left_bound) %>% min(., na.rm = TRUE)) - 2.5
    }


    if(ncomp == 2){
      plot_min_2 <- min(sim_pi_survreg_boot(df, fit = output$mu_model[[1]], alpha = 0.05, nSims = 10000) %>% pull(lwr) %>% min(., na.rm = TRUE) - 0.2,
                        sim_pi_survreg_boot(df, fit = output$mu_model[[2]], alpha = 0.05, nSims = 10000) %>% pull(lwr) %>% min(., na.rm = TRUE) - 0.2)
    } else if(ncomp == 1){
      plot_min_2 <- sim_pi_survreg_boot(df, fit = fitted_comp, alpha = 0.05, nSims = 10000) %>% pull(lwr) %>% min(., na.rm = TRUE) - 0.2
    }else{
      plot_min_2 = plot_min_1
    }

    plot_min = min(plot_min_1, plot_min_2, na.rm = TRUE)
    if(range_zoom){
      return(plot_min_1)
    }else{
    return(plot_min)
    }
  } else if(side == "max"){
    if(nrow(df %>% filter(right_bound == Inf)) > 0){
      plot_max_1 <- (df %>% filter(right_bound == Inf) %>% pull(left_bound) %>% max(., na.rm = TRUE)) + 2.5
    }else{
      plot_max_1 <- (df %>% pull(right_bound) %>% max(., na.rm = TRUE)) + 2.5
    }

    if(ncomp == 1){
      plot_max_2 <- sim_pi_survreg_boot(df, fit = fitted_comp, alpha = 0.05, nSims = 10000) %>% pull(upr) %>% max(., na.rm = TRUE) + 0.2
    } else if(ncomp == 2){
      plot_max_2 <- max(sim_pi_survreg_boot(df, fit = output$mu_model[[1]], alpha = 0.05, nSims = 10000) %>% pull(upr) %>% max(., na.rm = TRUE) + 0.2,
                        sim_pi_survreg_boot(df, fit = output$mu_model[[2]], alpha = 0.05, nSims = 10000) %>% pull(upr) %>% max(., na.rm = TRUE) + 0.2)
    }else{
      plot_max_2 = plot_max_1
    }

    plot_max = max(plot_max_1, plot_max_2, na.rm = TRUE)
    if(range_zoom){
      return(plot_max_1)
    }else{
      return(plot_max)
    }
  }else{
    errorCondition("choose 'min' or 'max'")
  }
}

check_comp_conv = function(models){
  is.na(models$scale) | (tibble(a = models$coefficients) %>% filter(is.na(a)) %>% nrow) > 0
}

