#' Title
#'
#' @param binom_model
#' @param newmodel
#' @param ncomp
#' @param possible_data
#' @param prior_step_plot
#' @param i
#'
#' @return
#' @export
#'
#' @examples
plot_fm_step = function(binom_model, newmodel, ncomp, possible_data, prior_step_plot, i){

  data.plot = possible_data %>% mutate(cens =
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

  if(nrow(data.plot %>% filter(left_bound == -Inf)) > 0){
    plot_min_1 <- (data.plot %>% filter(left_bound == -Inf) %>% pull(right_bound) %>% min) - 1
  }else{
    plot_min_1 <- (data.plot %>% pull(left_bound) %>% min) - 1
  }


  if(ncomp == 1){
    plot_min_2 <- sim_pi_survreg_boot(data.plot, fit = newmodel, alpha = 0.05, nSims = 10000) %>% pull(lwr) %>% min - 0.2
  } else if(ncomp == 2){
    plot_min_2 <- min(sim_pi_survreg_boot(data.plot, fit = newmodel[[1]], alpha = 0.05, nSims = 10000) %>% pull(lwr) %>% min - 0.2,
                      sim_pi_survreg_boot(data.plot, fit = newmodel[[2]], alpha = 0.05, nSims = 10000) %>% pull(lwr) %>% min - 0.2)
  }else{
    plot_min_2 = plot_min_1
  }

  plot_min = min(plot_min_1, plot_min_2)


  if(nrow(data.plot %>% filter(right_bound == Inf)) > 0){
    plot_max_1 <- (data.plot %>% filter(right_bound == Inf) %>% pull(left_bound) %>% max) + 1
  }else{
    plot_max_1 <- (data.plot %>% pull(right_bound) %>% max) + 1
  }

  if(ncomp == 1){
    plot_max_2 <- sim_pi_survreg_boot(data.plot, fit = newmodel, alpha = 0.05, nSims = 10000) %>% pull(upr) %>% max + 0.2
  } else if(ncomp == 2){
    plot_max_2 <- max(sim_pi_survreg_boot(data.plot, fit = newmodel[[1]], alpha = 0.05, nSims = 10000) %>% pull(upr) %>% max + 0.2,
                      sim_pi_survreg_boot(data.plot, fit = newmodel[[2]], alpha = 0.05, nSims = 10000) %>% pull(upr) %>% max + 0.2)
  }else{
    plot_max_2 = plot_max_1
  }

  plot_max = max(plot_max_1, plot_max_2)


  #ciTools::add_pi(data.plot, newmodel[[1]], alpha = 0.05, names = c("lwr", "upr"))
  #doesn't work with gaussian dist


  #mu.se.brd <- function(t, c, z, newmodel){predict(newmodel[[c]], data.frame(t = t)) + z * predict(newmodel[[c]], data.frame(t = t), se = TRUE)$se.fit}
  #mu.se.brd.fms <- function(t, z, newmodel){predict(newmodel, data.frame(t = t)) + z * predict(newmodel, data.frame(t = t), se = TRUE)$se.fit}

  if(ncomp == 2){

    #newmodel[[1]]$scale %>% print
    #newmodel[[2]]$scale %>% print

    ci_data <- tibble(t = rep(seq(0, max(possible_data$t), len = 300), 2)) %>%
      mutate(
        c1pred = predict(newmodel[[1]], tibble(t), se = T)$fit,
        c1pred_se = predict(newmodel[[1]], tibble(t), se = T)$se.fit,
        c1pred_lb = c1pred - 1.96 * c1pred_se,
        c1pred_ub = c1pred + 1.96 * c1pred_se,
        c2pred = predict(newmodel[[2]], tibble(t), se = T)$fit,
        c2pred_se = predict(newmodel[[2]], tibble(t), se = T)$se.fit,
        c2pred_lb = c2pred - 1.96 * c2pred_se,
        c2pred_ub = c2pred + 1.96 * c2pred_se,
      )

    mean <- data.plot %>% ggplot() +
      #geom_bar(aes(x = mid, fill = cens)) +
      geom_point(aes(x = t, y = mid, color = `P(C=c|y,t)`), data = data.plot %>% filter(c == "2"), alpha = 0) +
      geom_segment(aes(x = t, xend = t, y = left_bound, yend = right_bound, color = `P(C=c|y,t)`), data = (data.plot %>% filter(cens == "int" & c == "2")), alpha = 0.3) +
      geom_segment(aes(x = t, xend = t, y = right_bound, yend = left_bound, color = `P(C=c|y,t)`), data = (data.plot %>% filter(cens == "lc" & c == "2") %>% mutate(plot_min)), arrow = arrow(length = unit(0.03, "npc")), alpha = 0.3) +
      geom_segment(aes(x = t, xend = t, y = left_bound, yend = right_bound, color = `P(C=c|y,t)`), data = (data.plot %>% filter(cens == "rc" & c == "2") %>% mutate(plot_max)), arrow = arrow(length = unit(0.03, "npc")), alpha = 0.3) +
      geom_point(aes(x = t, y = left_bound,  color = `P(C=c|y,t)`), data = data.plot %>% filter(left_bound != -Inf & c == "2"), alpha = 0.3) +
      geom_point(aes(x = t, y = right_bound,  color = `P(C=c|y,t)`), data = data.plot %>% filter(right_bound != Inf & c == "2"), alpha = 0.3) +
      scale_colour_gradientn(colours = c("purple", "darkorange")) +
      #ylim(plot_min - 0.5, plot_max + 0.5) +
      ggtitle(paste0("Iteration ", i)) +
      xlab("Time") +
      ylab("MIC") +
      ggnewscale::new_scale_color() +
      ylim(plot_min, plot_max)
    #geom_function(fun = function(t){mu.se.brd(t, c = 1, z = 1.96)}, aes(color = "Component 1 Mu", linetype = "Fitted Model SE"), size = 0.6, alpha = 0.6) +
    #geom_function(fun = function(t){mu.se.brd(t, c = 1, z = -1.96)}, aes(color = "Component 1 Mu", linetype = "Fitted Model SE"), size = 0.6, alpha = 0.6) +
    #geom_function(fun = function(t){mu.se.brd(t, c = 2, z = 1.96)}, aes(color = "Component 2 Mu", linetype = "Fitted Model SE"), size = 0.6, alpha = 0.6) +
    #geom_function(fun = function(t){mu.se.brd(t, c = 2, z = -1.96)}, aes(color = "Component 2 Mu", linetype = "Fitted Model SE"), size = 0.6, alpha = 0.6) +
    if(!is.na(newmodel[[1]]$scale)){
      mean = mean +
        geom_function(fun = function(t){predict(newmodel[[1]], newdata = data.frame(t = t))}, aes(color = "Component 1 Mu", linetype = "Fitted Model")) +
        geom_ribbon(aes(ymin = c1pred_lb, ymax = c1pred_ub, x = t, fill = "Component 1 Mu"), data = ci_data, alpha = 0.25) +
        geom_ribbon(aes(ymin = lwr, ymax = upr, x = t, fill = "Component 1 Mu"), data = sim_pi_survreg_boot(data.plot, fit = newmodel[[1]], alpha = 0.05, nSims = 10000), alpha = 0.15)
    }
    if(i > 1 & prior_step_plot && !is.na(oldmodel[[1]]$scale)){
      mean = mean + geom_function(fun = function(t){predict(oldmodel[[1]], newdata = data.frame(t = t))}, aes(color = "Component 1 Mu Prior", linetype = "Fitted Model"))
    }

    if(!is.na(newmodel[[2]]$scale)){
      mean = mean +
        geom_function(fun = function(t){predict(newmodel[[2]], newdata = data.frame(t = t))}, aes(color = "Component 2 Mu", linetype = "Fitted Model")) +
        geom_ribbon(aes(ymin = c2pred_lb, ymax = c2pred_ub, x = t, fill = "Component 2 Mu"), data = ci_data, alpha = 0.25) +
        geom_ribbon(aes(ymin = lwr, ymax = upr, x = t, fill = "Component 2 Mu"), data = sim_pi_survreg_boot(data.plot, fit = newmodel[[2]], alpha = 0.05, nSims = 10000), alpha = 0.15)
    }
    if(i > 1 & prior_step_plot && !is.na(oldmodel[[2]]$scale)){
      mean = mean + geom_function(fun = function(t){predict(oldmodel[[2]], newdata = data.frame(t = t))}, aes(color = "Component 2 Mu Prior", linetype = "Fitted Model"))
    }

    ##find sim_pi_survreg_boot in scratch_add_pi_survreg.R

    # need to examine the things for sim_pi_survreg_boot, specifically the vcov stuff and if we should let it draw values for all spline terms and then also for the
    #way it calculates the sim response
    #do we need to account for weighting or anything?

    pi <- ggplot() +
      geom_function(fun = function(t){predict(binom_model, newdata = data.frame(t = t), type = "response")}, aes(color = "Current Step")) +
      xlim(0, 16) +
      ylim(0,1)

    if(prior_step_plot & i > 1){
      pi = pi + geom_function(fun = function(t){predict(oldbinommodel, newdata = data.frame(t = t), type = "response")}, aes(color = "Prior Step"))
    }



    return(mean/pi)

  }else{

    ci_data <- tibble(t = rep(seq(0, max(possible_data$t), len = 300), 2)) %>%
      mutate(
        c1pred = predict(newmodel, tibble(t), se = T)$fit,
        c1pred_se = predict(newmodel, tibble(t), se = T)$se.fit,
        c1pred_lb = c1pred - 1.96 * c1pred_se,
        c1pred_ub = c1pred + 1.96 * c1pred_se
      )

    #newmodel$scale %>% print()

    mean <- data.plot %>% ggplot() +
      #geom_bar(aes(x = mid, fill = cens)) +
      #geom_point(aes(x = t, y = mid, color = `P(C=c|y,t)`), data = data.plot %>% filter(c == "2"), alpha = 0) +
      geom_segment(aes(x = t, xend = t, y = left_bound, yend = right_bound, color = cens), data = (data.plot %>% filter(cens == "int")), alpha = 0.2) +
      geom_segment(aes(x = t, xend = t, y = right_bound, yend = left_bound, color = cens), data = (data.plot %>% filter(cens == "lc") %>% mutate(left_bound = plot_min)), arrow = arrow(length = unit(0.03, "npc")), alpha = 0.2) +
      geom_segment(aes(x = t, xend = t, y = left_bound, yend = right_bound, color = cens), data = (data.plot %>% filter(cens == "rc") %>% mutate(right_bound = plot_max)), arrow = arrow(length = unit(0.03, "npc")), alpha = 0.2) +
      geom_point(aes(x = t, y = left_bound,  color = cens), data = data.plot %>% filter(left_bound != -Inf), alpha = 0.2) +
      geom_point(aes(x = t, y = right_bound,  color = cens), data = data.plot %>% filter(right_bound != Inf), alpha = 0.2) +
      #scale_colour_gradientn(colours = c("purple", "orange")) +
      #ylim(plot_min - 0.5, plot_max + 0.5) +
      ggtitle(paste0("Iteration ", i)) +
      xlab("Time") +
      ylab("MIC") +
      ggnewscale::new_scale_color() +
      geom_function(fun = function(t){predict(newmodel, newdata = data.frame(t = t))}, aes(color = "Component Mu", linetype = "Fitted Model")) +
      #geom_function(fun = function(t){predict(newmodel[[2]], newdata = data.frame(t = t))}, aes(color = "Component 2 Mu", linetype = "Fitted Model")) +
      #geom_function(fun = function(t){mu.se.brd(t, c = 1, z = 1.96)}, aes(color = "Component 1 Mu", linetype = "Fitted Model SE"), size = 0.6, alpha = 0.6) +
      #geom_function(fun = function(t){mu.se.brd(t, c = 1, z = -1.96)}, aes(color = "Component 1 Mu", linetype = "Fitted Model SE"), size = 0.6, alpha = 0.6) +
      #geom_function(fun = function(t){mu.se.brd.fms(t, z = 1.96)}, aes(color = "Component Mu", linetype = "Fitted Model SE"), size = 0.6, alpha = 0.6) +
      #geom_function(fun = function(t){mu.se.brd.fms(t, z = -1.96)}, aes(color = "Component Mu", linetype = "Fitted Model SE"), size = 0.6, alpha = 0.6) +
      geom_ribbon(aes(ymin = c1pred_lb, ymax = c1pred_ub, x = t, fill = "Component 1 Mu"), data = ci_data, alpha = 0.2) +
      geom_ribbon(aes(ymin = lwr, ymax = upr, x = t, fill = "Component 1 Mu"), data = sim_pi_survreg_boot(data.plot, fit = newmodel, alpha = 0.05, nSims = 10000), alpha = 0.15) +
      ylim(plot_min, plot_max)


    mean

  }

}
