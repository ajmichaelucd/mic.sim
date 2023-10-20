#' Title
#'
#' @param output
#' @param title
#' @param add_log_reg
#' @param s_breakpoint
#' @param r_breakpoint
#'
#' @return
#' @export
#'
#' @examples
plot_fm <- function(output, title, add_log_reg, s_breakpoint, r_breakpoint){

    if(output$ncomp == "2"){
      check_comp_conv = function(models){
        is.na(models$scale) | (tibble(a = models$coefficients) %>% filter(is.na(a)) %>% nrow) > 0
      }
      results <- tibble(c = 1:2, dnc = purrr::map_lgl(output$newmodel, ~check_comp_conv(.x)))
      if(nrow(results %>% filter(dnc)) > 0){
        fitted_comp = output$newmodel[[results %>% filter(!dnc) %>% pull(c)]]
        ncomp = 1
      } else{
        ncomp = 2
      }
    }else{
      fitted_comp = output$newmodel
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

  if(nrow(df %>% filter(left_bound == -Inf)) > 0){
    plot_min_1 <- (df %>% filter(left_bound == -Inf) %>% pull(right_bound) %>% min) - 1
  }else{
    plot_min_1 <- (df %>% pull(left_bound) %>% min) - 1
  }


  if(ncomp == 1){
    plot_min_2 <- sim_pi_survreg_boot(df, fit = fitted_comp, alpha = 0.05, nSims = 10000) %>% pull(lwr) %>% min - 0.2
  } else if(ncomp == 2){
    plot_min_2 <- min(sim_pi_survreg_boot(df, fit = output$newmodel[[1]], alpha = 0.05, nSims = 10000) %>% pull(lwr) %>% min - 0.2,
                      sim_pi_survreg_boot(df, fit = output$newmodel[[2]], alpha = 0.05, nSims = 10000) %>% pull(lwr) %>% min - 0.2)
  }else{
    plot_min_2 = plot_min_1
  }

  plot_min = min(plot_min_1, plot_min_2)


  if(nrow(df %>% filter(right_bound == Inf)) > 0){
    plot_max_1 <- (df %>% filter(right_bound == Inf) %>% pull(left_bound) %>% max) + 1
  }else{
    plot_max_1 <- (df %>% pull(right_bound) %>% max) + 1
  }

  if(ncomp == 1){
    plot_max_2 <- sim_pi_survreg_boot(df, fit = fitted_comp, alpha = 0.05, nSims = 10000) %>% pull(upr) %>% max + 0.2
  } else if(ncomp == 2){
    plot_max_2 <- max(sim_pi_survreg_boot(df, fit = output$newmodel[[1]], alpha = 0.05, nSims = 10000) %>% pull(upr) %>% max + 0.2,
                      sim_pi_survreg_boot(df, fit = output$newmodel[[2]], alpha = 0.05, nSims = 10000) %>% pull(upr) %>% max + 0.2)
  }else{
    plot_max_2 = plot_max_1
  }

  plot_max = max(plot_max_1, plot_max_2)


  #ciTools::add_pi(df, output$newmodel[[1]], alpha = 0.05, names = c("lwr", "upr"))
  #doesn't work with gaussian dist


  mu.se.brd <- function(t, c, z){predict(output$newmodel[[c]], data.frame(t = t)) + z * predict(output$newmodel[[c]], data.frame(t = t), se = TRUE)$se.fit}
  mu.se.brd.fms <- function(t, z){predict(fitted_comp, data.frame(t = t)) + z * predict(fitted_comp, data.frame(t = t), se = TRUE)$se.fit}

  if(ncomp == 2){

    output$newmodel[[1]]$scale %>% print
    output$newmodel[[2]]$scale %>% print

    ci_data <- tibble(t = rep(seq(0, max(output$possible_data$t), len = 300), 2)) %>%
      mutate(
        c1pred = predict(output$newmodel[[1]], tibble(t), se = T)$fit,
        c1pred_se = predict(output$newmodel[[1]], tibble(t), se = T)$se.fit,
        c1pred_lb = c1pred - 1.96 * c1pred_se,
        c1pred_ub = c1pred + 1.96 * c1pred_se,
        c2pred = predict(output$newmodel[[2]], tibble(t), se = T)$fit,
        c2pred_se = predict(output$newmodel[[2]], tibble(t), se = T)$se.fit,
        c2pred_lb = c2pred - 1.96 * c2pred_se,
        c2pred_ub = c2pred + 1.96 * c2pred_se,
      )

    mean <- df %>% ggplot() +
      #geom_bar(aes(x = mid, fill = cens)) +
      geom_function(fun = function(t){predict(output$newmodel[[1]], newdata = data.frame(t = t))}, aes(color = "Component 1 Mu", linetype = "Fitted Model")) +
      geom_function(fun = function(t){predict(output$newmodel[[2]], newdata = data.frame(t = t))}, aes(color = "Component 2 Mu", linetype = "Fitted Model")) +
      geom_ribbon(aes(ymin = c1pred_lb, ymax = c1pred_ub, x = t, fill = "Component 1 Mu"), data = ci_data, alpha = 0.25) +
      geom_ribbon(aes(ymin = c2pred_lb, ymax = c2pred_ub, x = t, fill = "Component 2 Mu"), data = ci_data, alpha = 0.25) +
      geom_ribbon(aes(ymin = lwr, ymax = upr, x = t, fill = "Component 1 Mu"), data = sim_pi_survreg_boot(df, fit = output$newmodel[[1]], alpha = 0.05, nSims = 10000), alpha = 0.15) +
      geom_ribbon(aes(ymin = lwr, ymax = upr, x = t, fill = "Component 2 Mu"), data = sim_pi_survreg_boot(df, fit = output$newmodel[[2]], alpha = 0.05, nSims = 10000), alpha = 0.15) +
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
      ylab("MIC") +

      #geom_function(fun = function(t){mu.se.brd(t, c = 1, z = 1.96)}, aes(color = "Component 1 Mu", linetype = "Fitted Model SE"), size = 0.6, alpha = 0.6) +
      #geom_function(fun = function(t){mu.se.brd(t, c = 1, z = -1.96)}, aes(color = "Component 1 Mu", linetype = "Fitted Model SE"), size = 0.6, alpha = 0.6) +
      #geom_function(fun = function(t){mu.se.brd(t, c = 2, z = 1.96)}, aes(color = "Component 2 Mu", linetype = "Fitted Model SE"), size = 0.6, alpha = 0.6) +
      #geom_function(fun = function(t){mu.se.brd(t, c = 2, z = -1.96)}, aes(color = "Component 2 Mu", linetype = "Fitted Model SE"), size = 0.6, alpha = 0.6) +
      ylim(plot_min, plot_max)

    ##find sim_pi_survreg_boot in scratch_add_pi_survreg.R

    # need to examine the things for sim_pi_survreg_boot, specifically the vcov stuff and if we should let it draw values for all spline terms and then also for the
    #way it calculates the sim response
    #do we need to account for weighting or anything?

    pi <- ggplot() +
      geom_function(fun = function(t){(1 - predict(output$binom_model, newdata = data.frame(t = t), type = "response"))}, aes(color = "Component 1")) +
      geom_function(fun = function(t){predict(output$binom_model, newdata = data.frame(t = t), type = "response")}, aes(color = "Component 2")) +
      xlim(0, 16) +
      ylim(0,1)
    if(add_log_reg & !is.null(s_breakpoint) & !is.null(r_breakpoint)){
      lr_output = log_reg(output$possible_data, data_type = "possible_data", drug = NULL, date_col = "t", date_type = "decimal", first_year = NULL, s_breakpoint = s_breakpoint, r_breakpoint = r_breakpoint)

      pi = pi +
        geom_function(fun = function(t){(1 - predict(lr_output, newdata = data.frame(t = t), type = "response"))}, aes(color = "Component 1", linetype = "Logistic Regression")) +
        geom_function(fun = function(t){predict(lr_output, newdata = data.frame(t = t), type = "response")}, aes(color = "Component 2", linetype = "Logistic Regression"))
      mean = mean +
        ggnewscale::new_scale_color() +
        geom_hline(aes(yintercept = s_breakpoint %>% parse_number() %>% log2, color = "Susceptible Breakpoint"), alpha = 0.4) +
        geom_hline(aes(yintercept = r_breakpoint %>% parse_number() %>% log2, color = "Resistant Breakpoint"), alpha = 0.4) +
        scale_color_viridis_d(option = "turbo")
    }
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
        geom_ribbon(aes(ymin = c1pred_lb, ymax = c1pred_ub, x = t, fill = "Component 1 Mu"), data = ci_data, alpha = 0.2) +
        geom_ribbon(aes(ymin = lwr, ymax = upr, x = t, fill = "Component 1 Mu"), data = sim_pi_survreg_boot(df, fit = fitted_comp, alpha = 0.05, nSims = 10000), alpha = 0.15) +
        ggnewscale::new_scale_color() +
        #geom_point(aes(x = t, y = mid, color = `P(C=c|y,t)`), data = df %>% filter(c == "2"), alpha = 0) +
        geom_segment(aes(x = t, xend = t, y = left_bound, yend = right_bound, color = cens), data = (df %>% filter(cens == "int")), alpha = 0.2) +
        geom_segment(aes(x = t, xend = t, y = right_bound, yend = left_bound, color = cens), data = (df %>% filter(cens == "lc") %>% mutate(left_bound = plot_min)), arrow = arrow(length = unit(0.03, "npc")), alpha = 0.2) +
        geom_segment(aes(x = t, xend = t, y = left_bound, yend = right_bound, color = cens), data = (df %>% filter(cens == "rc") %>% mutate(right_bound = plot_max)), arrow = arrow(length = unit(0.03, "npc")), alpha = 0.2) +
        geom_point(aes(x = t, y = left_bound,  color = cens), data = df %>% filter(left_bound != -Inf), alpha = 0.2) +
        geom_point(aes(x = t, y = right_bound,  color = cens), data = df %>% filter(right_bound != Inf), alpha = 0.2) +
        #scale_colour_gradientn(colours = c("purple", "orange")) +
        #ylim(plot_min - 0.5, plot_max + 0.5) +
        ggtitle(title) +
        xlab("Time") +
        ylab("MIC") +
        #geom_function(fun = function(t){predict(fitted_comp[[2]], newdata = data.frame(t = t))}, aes(color = "Component 2 Mu", linetype = "Fitted Model")) +
        #geom_function(fun = function(t){mu.se.brd(t, c = 1, z = 1.96)}, aes(color = "Component 1 Mu", linetype = "Fitted Model SE"), size = 0.6, alpha = 0.6) +
        #geom_function(fun = function(t){mu.se.brd(t, c = 1, z = -1.96)}, aes(color = "Component 1 Mu", linetype = "Fitted Model SE"), size = 0.6, alpha = 0.6) +
        ylim(plot_min, plot_max)

      return(mean)

    }else{
      ci_data <- tibble(t = rep(seq(0, max(output$possible_data$t), len = 300), 2)) %>%
        mutate(
          c1pred = predict(fitted_comp, tibble(t), se = T)$fit,
          c1pred_se = predict(fitted_comp, tibble(t), se = T)$se.fit,
          c1pred_lb = c1pred - 1.96 * c1pred_se,
          c1pred_ub = c1pred + 1.96 * c1pred_se
        )

      mean <- df %>% ggplot() +
        #geom_bar(aes(x = mid, fill = cens)) +
        geom_function(fun = function(t){predict(fitted_comp, newdata = data.frame(t = t))}, aes(color = "Component Mu", linetype = "Fitted Model")) +
        geom_function(fun = function(t){mu.se.brd.fms(t, z = 1.96)}, aes(color = "Component Mu", linetype = "Fitted Model SE"), size = 0.6, alpha = 0.6) +
        geom_function(fun = function(t){mu.se.brd.fms(t, z = -1.96)}, aes(color = "Component Mu", linetype = "Fitted Model SE"), size = 0.6, alpha = 0.6) +
        geom_ribbon(aes(ymin = c1pred_lb, ymax = c1pred_ub, x = t, fill = "Component 1 Mu"), data = ci_data, alpha = 0.2) +
        geom_ribbon(aes(ymin = lwr, ymax = upr, x = t, fill = "Component 1 Mu"), data = sim_pi_survreg_boot(df, fit = fitted_comp, alpha = 0.05, nSims = 10000), alpha = 0.15) +
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
        xlab("Time") +
        ylab("MIC") +
        #geom_function(fun = function(t){predict(fitted_comp[[2]], newdata = data.frame(t = t))}, aes(color = "Component 2 Mu", linetype = "Fitted Model")) +
        #geom_function(fun = function(t){mu.se.brd(t, c = 1, z = 1.96)}, aes(color = "Component 1 Mu", linetype = "Fitted Model SE"), size = 0.6, alpha = 0.6) +
        #geom_function(fun = function(t){mu.se.brd(t, c = 1, z = -1.96)}, aes(color = "Component 1 Mu", linetype = "Fitted Model SE"), size = 0.6, alpha = 0.6) +
        ylim(plot_min, plot_max)

      pi <- ggplot() +
        geom_function(fun = function(t){(1 - predict(output$binom_model, newdata = data.frame(t = t), type = "response"))}, aes(color = "Component 1", linetype = "Model")) +
        geom_function(fun = function(t){predict(output$binom_model, newdata = data.frame(t = t), type = "response")}, aes(color = "Component 2", linetype = "Model")) +
        xlim(0, 16) +
        ylim(0,1)


      if(add_log_reg & !is.null(s_breakpoint) & !is.null(r_breakpoint)){
        lr_output = log_reg(output$possible_data, data_type = "possible_data", drug = NULL, date_col = "t", date_type = "decimal", first_year = NULL, s_breakpoint = s_breakpoint, r_breakpoint = r_breakpoint)

pi = pi +
  geom_function(fun = function(t){(1 - predict(lr_output, newdata = data.frame(t = t), type = "response"))}, aes(color = "Component 1", linetype = "Logistic Regression")) +
  geom_function(fun = function(t){predict(lr_output, newdata = data.frame(t = t), type = "response")}, aes(color = "Component 2", linetype = "Logistic Regression"))
mean = mean +
  ggnewscale::new_scale_color() +
  geom_hline(aes(yintercept = s_breakpoint %>% parse_number() %>% log2, color = "Susceptible Breakpoint"), alpha = 0.4) +
  geom_hline(aes(yintercept = r_breakpoint %>% parse_number() %>% log2, color = "Resistant Breakpoint"), alpha = 0.4) +
  scale_color_viridis_d(option = "turbo")
      }
      return(mean/pi)
    }
  }


  ##maxing out iterations fails to generate a `converge` object!!!!!!!!!!!!!!

}
