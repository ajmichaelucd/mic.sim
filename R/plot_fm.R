#' Plot Output of Model Fitting
#'
#' Takes output of a fit_EM() run and plots it.
#' Logistic regression fitting is in progress (currently does not accept covariates and non-linear term must be "t")
#'
#' @param output List, output of a fit_EM()
#' @param title String, title of plot
#' @param add_log_reg Logical, add a curve to the pi (component weight) plot showing proportion resistant and susceptible
#' @param ecoff Numeric or String, represents an ECOFF on the MIC Scale to define the upper limit of the WT component, A number is interpreted as WT being <= ECOFF.
#' @param s_breakpoint String, represents S breakpoint (e.g. <= 2) on MIC scale (not log2 scale)
#' @param r_breakpoint String, represents R breakpoint (e.g. >= 32) on MIC scale (not log2 scale)
#' @param visual_split Numeric or String, represents a visual split point on the MIC scale to define the upper limit of the WT component. A number is interpreted as WT being <= visual_split
#' @param use_prior_step Logical, if one mu model did not converge, can try plotting mu models from previous step by setting this to TRUE
#' @param range_zoom Logical, zoom y axis to range of tested concentrations
#' @param plot_range Vector of length 2, minimum and maximum values of y axis of plot
#' @param start_date Integer, value at which x axis should start (year).
#' @param  x_axis_t_breaks Numerical vector, vector of values on the scale of t, the time variable in years from the start of the study. Helpful to use seq(0,t_max, by = spacing) where t_max is the length of study period and spacing is how many years to separate major ticks by
#' @param skip Vector, vector of either "ecoff", "bkpts", or c("ecoff", "bkpts"), to describe any splits for which logistic regression should not be plotted if another logistic regression is being plotted. If only one divider is used, just turn off add_log_reg
#'
#' @import ggplot2
#' @import ggnewscale
#' @importFrom patchwork wrap_plots
#' @importFrom latex2exp TeX
#'
#' @return
#' @export
#'
#' @examples
#' data = simulate_mics()
#' output = fit_EM(model = "pspline",
#' approach = "full",
#' pre_set_degrees = c(4,4),
#' visible_data = data,
#' non_linear_term = "t",
#' covariates = NULL,
#' pi_formula = c == "2" ~ s(t),
#' max_it = 300,
#' ncomp = 2,
#' tol_ll = 1e-6,
#' pi_link = "logit",
#' verbose = 1,
#' model_coefficient_tolerance = 0.00001,
#' initial_weighting = 3,
#' sd_initial = 0.2
#' )
#' plot_fm(output = output, title = "Example", add_log_reg = TRUE, s_breakpoint = "<=1", r_breakpoint = ">=4")
#'
#'
plot_fm <- function(output, title ="", add_log_reg = FALSE, ecoff = NA, s_breakpoint = NA, r_breakpoint = NA, visual_split = NA, use_prior_step = FALSE, range_zoom = FALSE, plot_range = NULL, start_date = 0, x_axis_t_breaks = NULL, skip = NULL){



  if(!is.null(output$prior_step_models) & use_prior_step){
    output$mu_model = output$prior_step_models$mu_models
  }

  if(!is.null(output$fixed_side)){
    if(output$fixed_side == "LC"){
      check = check_comp_conv(output$mu_model[[1]])
      dnc = c(TRUE, check)
      ncomp = 1 - check
      results = tibble(c = 1:2, dnc)
      fitted_comp = output$mu_model[[1]]
    }else if(output$fixed_side == "RC"){
      check = check_comp_conv(output$mu_model[[1]])
      dnc = c(check, TRUE)
      ncomp = 1 - check
      results = tibble(c = 1:2, dnc)
      fitted_comp = output$mu_model[[1]]
    }else{
      errorCondition("Invalid value for fixed_side")
    }
  }else{

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

  attr(df, "model") <- attr(output$possible_data, "model")




if(is.null(plot_range)){
plot_min <- plot_bounds(output$possible_data, "min", ncomp, range_zoom, output, fitted_comp)
plot_max <- plot_bounds(output$possible_data, "max", ncomp, range_zoom, output, fitted_comp)
}else{
  plot_min = plot_range[1]
  plot_max = plot_range[2]
}


#lc = output$possible_data$low_cons %>% min()
#hc = output$possible_data$high_cons %>% min()

  #ciTools::add_pi(df, output$mu_model[[1]], alpha = 0.05, names = c("lwr", "upr"))
  #doesn't work with gaussian dist


  mu.se.brd <- function(t, c, z){predict(output$mu_model[[c]], data.frame(t = t)) + (z * predict(output$mu_model[[c]], data.frame(t = t), se = TRUE)$se.fit)}
  mu.se.brd.fms <- function(t, z){predict(fitted_comp, data.frame(t = t)) + (z * predict(fitted_comp, data.frame(t = t), se = TRUE)$se.fit)}

  if(ncomp == 2){

    #output$mu_model[[1]]$scale %>% print
    #output$mu_model[[2]]$scale %>% print


ci_data = get_two_comp_ci(output)

mean <- df %>%
      offset_time_as_date_in_df(., start_date) %>%
      ggplot(aes(x = t)) +
  scale_colour_gradient2(high = "#00BFC4", low = "#F8766D", mid = "green", midpoint = 0.5, name = "P(C=2|y,t)") +
  #geom_point(aes(x = t, y = mid, color = `P(C=c|y,t)`), data = df %>% filter(c == "2"), alpha = 0) +
  geom_segment(aes(x = t, xend = t, y = left_bound, yend = right_bound, color = `P(C=c|y,t)`), data = (df %>% filter(cens == "int" & c == "2") %>% offset_time_as_date_in_df(., start_date)), alpha = 0.3) +
  geom_segment(aes(x = t, xend = t, y = right_bound, yend = left_bound, color = `P(C=c|y,t)`), data = (df %>% filter(cens == "lc" & c == "2") %>% mutate(plot_min) %>% offset_time_as_date_in_df(., start_date)), arrow = arrow(length = unit(0.03, "npc")), alpha = 0.3) +
  geom_segment(aes(x = t, xend = t, y = left_bound, yend = right_bound, color = `P(C=c|y,t)`), data = (df %>% filter(cens == "rc" & c == "2") %>% mutate(plot_max) %>% offset_time_as_date_in_df(., start_date)), arrow = arrow(length = unit(0.03, "npc")), alpha = 0.3) +
  geom_point(aes(x = t, y = left_bound,  color = `P(C=c|y,t)`), data = df %>% filter(left_bound != -Inf & c == "2") %>% offset_time_as_date_in_df(., start_date), alpha = 0.3) +
  geom_point(aes(x = t, y = right_bound,  color = `P(C=c|y,t)`), data = df %>% filter(right_bound != Inf & c == "2") %>% offset_time_as_date_in_df(., start_date), alpha = 0.3) +
  ggnewscale::new_scale_color() +

      #geom_bar(aes(x = mid, fill = cens)) +
      #geom_function(fun = function(t){predict(output$mu_model[[1]], newdata = data.frame(t = as_offset_time(x = t, start_date)))}, aes(color = "Component 1 Mu", linetype = "Fitted Model")) +
      #geom_function(fun = function(t){predict(output$mu_model[[2]], newdata = data.frame(t = as_offset_time(x = t, start_date)))}, aes(color = "Component 2 Mu", linetype = "Fitted Model")) +
      geom_line(aes(x = offset_time_as_date(t, start_date), y = c1pred, linetype = "Fitted Model", color = "Component 1 Mu"), data = ci_data) +
      geom_line(aes(x = offset_time_as_date(t, start_date), y = c2pred, linetype = "Fitted Model", color = "Component 2 Mu"), data = ci_data) +
      geom_ribbon(aes(ymin = c1pred_lb, ymax = c1pred_ub, x = offset_time_as_date(t, start_date), fill = "Component 1 Mu"), data = ci_data, alpha = 0.25) +
      geom_ribbon(aes(ymin = c2pred_lb, ymax = c2pred_ub, x = offset_time_as_date(t, start_date), fill = "Component 2 Mu"), data = ci_data, alpha = 0.25)
    if(attr(df, "model") != "mgcv"){
    mean = mean +
      geom_ribbon(aes(ymin = lwr, ymax = upr, x = t, fill = "Component 1 Mu"), data = sim_pi_survreg_boot(df, fit = output$mu_model[[1]], alpha = 0.05, nSims = 10000) %>% offset_time_as_date_in_df(., start_date), alpha = 0.15) +
      geom_ribbon(aes(ymin = lwr, ymax = upr, x = t, fill = "Component 2 Mu"), data = sim_pi_survreg_boot(df, fit = output$mu_model[[2]], alpha = 0.05, nSims = 10000) %>% offset_time_as_date_in_df(., start_date), alpha = 0.15) +
      scale_fill_manual(breaks = c("Component 1 Mu", "Component 2 Mu"), values = c("#e4190b", "#00999d"), labels = c(TeX(r'(Component 1 Mean: $\hat{\mu}_{1,t}$)'), TeX(r'(Component 2 Mean: $\hat{\mu}_{2,t}$)')), name = "Component Means")
    }
      mean = mean + scale_color_manual(breaks = c("Component 1 Mu", "Component 2 Mu"), values = c("#e4190b", "#00999d"), labels = c(TeX(r'(Component 1 Mean: $\hat{\mu}_{1,t}$)'), TeX(r'(Component 2 Mean: $\hat{\mu}_{2,t}$)')),name = "Component Means") +
      ggtitle(title) +
      xlab("Time") +
      ylab(bquote(log[2]~ MIC)) +
      ylim(plot_min - 1, plot_max + 1) +
      scale_y_continuous(breaks = scales::breaks_extended((plot_max - plot_min)/1.5)) +
      theme_minimal()

    ##find sim_pi_survreg_boot in scratch_add_pi_survreg.R

    # need to examine the things for sim_pi_survreg_boot, specifically the vcov stuff and if we should let it draw values for all spline terms and then also for the
    #way it calculates the sim response
    #do we need to account for weighting or anything?


    if(add_log_reg && (!is.na(ecoff) | (!is.na(s_breakpoint) & !is.na(r_breakpoint)) | !is.na(visual_split))){
      if(!is.na(s_breakpoint) & !is.na(r_breakpoint)){
      lr_output_bkpt = log_reg(output$possible_data, split_by = "r_breakpoint", data_type = "possible_data", drug = NULL, date_col = "t", date_type = "decimal", first_year = NULL, s_breakpoint = s_breakpoint, r_breakpoint = r_breakpoint)
      }
      if(!is.na(ecoff)){
        lr_output_ecoff = log_reg(output$possible_data, split_by = "ecoff", data_type = "possible_data", drug = NULL, date_col = "t", date_type = "decimal", first_year = NULL, ecoff = ecoff)
      }
      if(!is.na(visual_split)){
        lr_output_visual_split = log_reg(output$possible_data, split_by = "visual_split", data_type = "possible_data", drug = NULL, date_col = "t", date_type = "decimal", first_year = NULL, visual_split = visual_split)
      }

      pi_bounds = tibble(t = seq(0, max(output$possible_data$t), len = 300),
                         pi2 = predict(output$pi_model, newdata = data.frame(t = t), type = "response"),
                         pi1 = 1 - pi2,
                         pi_se = predict(output$pi_model, newdata = data.frame(t = t), se.fit = TRUE)$se.fit,
                         pi_1_lp = logit(1 - predict(output$pi_model, newdata = data.frame(t = t), type = "response")),
                         pi_2_lp = predict(output$pi_model, newdata = data.frame(t = t)),
                         pi_1_lb = inverse_logit(pi_1_lp - (1.96 * pi_se)),
                         pi_1_ub = inverse_logit(pi_1_lp + (1.96 * pi_se)),
                         pi_2_lb = inverse_logit(pi_2_lp - (1.96 * pi_se)),
                         pi_2_ub = inverse_logit(pi_2_lp + (1.96 * pi_se))
      )


      pi = df %>%
        offset_time_as_date_in_df(., start_date) %>%
        ggplot(aes(x = t)) +
        geom_line(aes(x = offset_time_as_date(t, start_date), y = pi1, color = "Component 1 Proportion", linetype = "Fitted Model"), data = pi_bounds) +
        geom_line(aes(x = offset_time_as_date(t, start_date), y = pi2, color = "Component 2 Proportion", linetype = "Fitted Model"), data = pi_bounds) +
        #geom_function(fun = function(t){(1 - predict(output$pi_model, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response"))}, aes(color = "Component 1 Proportion", linetype = "Fitted Model")) +
        #geom_function(fun = function(t){predict(output$pi_model, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response")}, aes(color = "Component 2 Proportion", linetype = "Fitted Model")) +
        #scale_color_manual(breaks = c("Component 1 Proportion", "Component 2 Proportion"), values = c("#e4190b", "#00BFC4"), name = "Component Prevalence") +
        ylim(0,1)  +
        xlab("Time") + ylab("Proportion") + theme_minimal()
      if(is.na(ecoff) & is.na(visual_split) &
         (!is.na(s_breakpoint) & !is.na(r_breakpoint))
      ){
        pi_bounds = pi_bounds %>%
          mutate(.by = t,
                 susceptible = 1 - predict(lr_output_bkpt, newdata = tibble(t = t), type = "response"),
                 resistant = predict(lr_output_bkpt, newdata = tibble(t = t), type = "response") )

        pi = pi +
          scale_color_manual(breaks = c("Component 1 Proportion", "Component 2 Proportion"), values = c("#e4190b", "#00999d"), labels = c(TeX(r'(Component 1 Prevalence: $\hat{\pi}_{1,t}$)'), TeX(r'(Component 2 Prevalence: $\hat{\pi}_{2,t}$)')), name = "Component Prevalence") +
          ggnewscale::new_scale_color() +
          geom_line(aes(x = offset_time_as_date(t, start_date), y = susceptible, color = "Susceptible", linetype = "Logistic Regression"), data = pi_bounds) +
          geom_line(aes(x = offset_time_as_date(t, start_date), y = resistant, color = "Resistant", linetype = "Logistic Regression"), data = pi_bounds) +
          #geom_function(fun = function(t){(1 - predict(lr_output_bkpt, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response"))}, aes(color = "Susceptible", linetype = "Logistic Regression")) +
          #geom_function(fun = function(t){predict(lr_output_bkpt, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response")}, aes(color = "Resistant", linetype = "Logistic Regression")) +
          scale_color_manual(breaks = c("Susceptible", "Resistant"), values = c("#7CAE00", "#C77CFF"), name = "Other Prevalence") + #+ guides(linetype = "none")
          scale_linetype_discrete(name = "Estimate Source")

        s_line = case_when(
          grepl("≤",s_breakpoint) ~ s_breakpoint %>% as.character() %>% parse_number() %>% log2,
          grepl("=",s_breakpoint) ~ s_breakpoint %>% as.character() %>% parse_number() %>% log2,
          grepl("<",s_breakpoint) ~ s_breakpoint %>% as.character() %>% parse_number() %>% log2 - 1,
          TRUE ~ s_breakpoint %>% as.character() %>% parse_number() %>% log2
        )
        r_line = case_when(
          grepl("≥",r_breakpoint) ~ r_breakpoint %>% as.character() %>% parse_number() %>% log2 - 1,
          grepl("=",r_breakpoint) ~ r_breakpoint %>% as.character() %>% parse_number() %>% log2 - 1,
          grepl(">",r_breakpoint) ~ r_breakpoint %>% as.character() %>% parse_number() %>% log2,
          TRUE ~ r_breakpoint %>% as.character() %>% parse_number() %>% log2 - 1
        )

        mean = mean +
          ggnewscale::new_scale_color() +
          geom_hline(aes(yintercept = s_line, color = "Susceptible Breakpoint", linetype =  "Breakpoint")) +
          geom_hline(aes(yintercept = r_line, color = "Resistant Breakpoint", linetype =  "Breakpoint")) +
          scale_color_manual(breaks = c("Susceptible Breakpoint", "Resistant Breakpoint"), labels = c(TeX(paste0("Susceptible Breakpoint: ", s_breakpoint,r'($\mu$)',"g/mL")), TeX(paste0("Resistant Breakpoint: ", r_breakpoint,r'($\mu$)',"g/mL"))), values = c("#7CAE00", "#C77CFF"), name = "Breakpoints and Cutoffs") +
          scale_linetype_manual(breaks=c("Fitted Model","Breakpoint"), values=c(1,5), guide = "none")  #+ guides(linetype = "none", color = "none")
      }else if(
        !is.na(ecoff) & is.na(visual_split) &
        (is.na(s_breakpoint) & is.na(r_breakpoint))
      ){
        pi_bounds = pi_bounds %>%
          mutate(.by = t,
                 wt = 1 - predict(lr_output_ecoff, newdata = tibble(t = t), type = "response"),
                 nwt = predict(lr_output_ecoff, newdata = tibble(t = t), type = "response") )


        pi = pi +
          scale_color_manual(breaks = c("Component 1 Proportion", "Component 2 Proportion"), values = c("#e4190b", "#00999d"), labels = c(TeX(r'(Component 1 Prevalence: $\hat{\pi}_{1,t}$)'), TeX(r'(Component 2 Prevalence: $\hat{\pi}_{2,t}$)')), name = "Component Prevalence") +
          ggnewscale::new_scale_color() +
          geom_line(aes(x = offset_time_as_date(t, start_date), y = wt, color = "WT", linetype = "Logistic Regression"), data = pi_bounds) +
          geom_line(aes(x = offset_time_as_date(t, start_date), y = nwt, color = "NWT", linetype = "Logistic Regression"), data = pi_bounds) +

          #geom_function(fun = function(t){(1 - predict(lr_output_ecoff, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response"))}, aes(color = "WT", linetype = "Logistic Regression")) +
          #geom_function(fun = function(t){predict(lr_output_ecoff, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response")}, aes(color = "NWT", linetype = "Logistic Regression")) +
          scale_color_manual(breaks = c("WT", "NWT"), values = c( "#fcbf07", "#0211a3"), name = "Other Prevalence")  + #guides(linetype = "none")
          scale_linetype_discrete(name = "Estimate Source")

        ecoff_line = case_when(
          grepl("≤", ecoff) ~ ecoff %>% as.character() %>% parse_number() %>% log2,
          grepl("=", ecoff) ~ ecoff %>% as.character() %>% parse_number() %>% log2,
          grepl("<", ecoff) ~ ecoff %>% as.character() %>% parse_number() %>% log2 - 1,
          TRUE ~ ecoff %>% as.character() %>% parse_number() %>% log2
        )

        mean = mean +
          ggnewscale::new_scale_color() +
          geom_hline(aes(yintercept = ecoff_line, color = "ECOFF", linetype =  "Cutoff")) +
          scale_color_manual(breaks = c("ECOFF"), values = c("#ffd700"), labels = c(TeX(paste0("ECOFF: ", ecoff,r'($\mu$)',"g/mL"))), name = "Breakpoints and Cutoffs") +
          scale_linetype_manual(breaks=c("Fitted Model","Cutoff"), values=c(1,2), guide = "none")  #+ guides(linetype = "none", color = "none")
      }else if(
        is.na(ecoff) & !is.na(visual_split) &
        (is.na(s_breakpoint) & is.na(r_breakpoint))
      ){
        pi_bounds = pi_bounds %>%
          mutate(.by = t,
                 c1vs = 1 - predict(lr_output_visual_split, newdata = tibble(t = t), type = "response"),
                 c2vs = predict(lr_output_visual_split, newdata = tibble(t = t), type = "response") )


        pi = pi +
          scale_color_manual(breaks = c("Component 1 Proportion", "Component 2 Proportion"), values = c("#e4190b", "#00999d"), labels = c(TeX(r'(Component 1 Prevalence: $\hat{\pi}_{1,t}$)'), TeX(r'(Component 2 Prevalence: $\hat{\pi}_{2,t}$)')), name = "Component Prevalence") +
          ggnewscale::new_scale_color() +
          geom_line(aes(x = offset_time_as_date(t, start_date), y = c1vs, color = "Below Split", linetype = "Logistic Regression"), data = pi_bounds) +
          geom_line(aes(x = offset_time_as_date(t, start_date), y = c2vs, color = "Above Split", linetype = "Logistic Regression"), data = pi_bounds) +

          #geom_function(fun = function(t){(1 - predict(lr_output_ecoff, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response"))}, aes(color = "WT", linetype = "Logistic Regression")) +
          #geom_function(fun = function(t){predict(lr_output_ecoff, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response")}, aes(color = "NWT", linetype = "Logistic Regression")) +
          scale_color_manual(breaks = c("Below Split", "Above Split"), values = c( "#DF4601", "#000000"), name = "Other Prevalence")  + #guides(linetype = "none")
          scale_linetype_discrete(name = "Estimate Source")

        visual_split_line = case_when(
          grepl("≤", visual_split) ~ visual_split %>% as.character() %>% parse_number() %>% log2,
          grepl("=", visual_split) ~ visual_split %>% as.character() %>% parse_number() %>% log2,
          grepl("<", visual_split) ~ visual_split %>% as.character() %>% parse_number() %>% log2 - 1,
          TRUE ~ visual_split %>% as.character() %>% parse_number() %>% log2
        )

        mean = mean +
          ggnewscale::new_scale_color() +
          geom_hline(aes(yintercept = visual_split_line, color = "Visual Split", linetype =  "Cutoff")) +
          scale_color_manual(breaks = c("Visual Split"), values = c("#DF4601"), labels = c(TeX(paste0("Visual Split: ", visual_split,r'($\mu$)',"g/mL"))), name = "Breakpoints and Cutoffs") +
          scale_linetype_manual(breaks=c("Fitted Model","Cutoff"), values=c(1,2), guide = "none")  #+ guides(linetype = "none", color = "none")

      }else if(
        !is.na(ecoff) & !is.na(visual_split) &
        (is.na(s_breakpoint) & is.na(r_breakpoint))
      ){
        pi_bounds = pi_bounds %>%
          mutate(.by = t,
                 wt = 1 - predict(lr_output_ecoff, newdata = tibble(t = t), type = "response"),
                 nwt = predict(lr_output_ecoff, newdata = tibble(t = t), type = "response"),
                 c1vs = 1 - predict(lr_output_visual_split, newdata = tibble(t = t), type = "response"),
                 c2vs = predict(lr_output_visual_split, newdata = tibble(t = t), type = "response") )



        if(!is.null(skip) && "ecoff" %in% skip){
          pi = pi +
            scale_color_manual(breaks = c("Component 1 Proportion", "Component 2 Proportion"), values = c("#e4190b", "#00999d"), labels = c(TeX(r'(Component 1 Prevalence: $\hat{\pi}_{1,t}$)'), TeX(r'(Component 2 Prevalence: $\hat{\pi}_{2,t}$)')), name = "Component Prevalence") +
            ggnewscale::new_scale_color() +
            geom_line(aes(x = offset_time_as_date(t, start_date), y = c1vs, color = "Below Split", linetype = "Logistic Regression"), data = pi_bounds) +
            geom_line(aes(x = offset_time_as_date(t, start_date), y = c2vs, color = "Above Split", linetype = "Logistic Regression"), data = pi_bounds) +
            #geom_function(fun = function(t){(1 - predict(lr_output_bkpt, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response"))}, aes(color = "Below Split", linetype = "Logistic Regression")) +
            #geom_function(fun = function(t){predict(lr_output_bkpt, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response")}, aes(color = "Resistant", linetype = "Logistic Regression")) +
            #geom_function(fun = function(t){(1 - predict(lr_output_ecoff, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response"))}, aes(color = "WT", linetype = "Logistic Regression")) +
            #geom_function(fun = function(t){predict(lr_output_ecoff, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response")}, aes(color = "NWT", linetype = "Logistic Regression")) +
            scale_color_manual(breaks = c("Below Split", "Above Split"), values = c( "#DF4601", "#000000"), name = "Other Prevalence") +  #+ guides(linetype = "none")
            scale_linetype_discrete(name = "Estimate Source")
        }else{
          pi = pi +
            scale_color_manual(breaks = c("Component 1 Proportion", "Component 2 Proportion"), values = c("#e4190b", "#00999d"), labels = c(TeX(r'(Component 1 Prevalence: $\hat{\pi}_{1,t}$)'), TeX(r'(Component 2 Prevalence: $\hat{\pi}_{2,t}$)')), name = "Component Prevalence") +
            ggnewscale::new_scale_color() +
            geom_line(aes(x = offset_time_as_date(t, start_date), y = c1vs, color = "Below Split", linetype = "Logistic Regression"), data = pi_bounds) +
            geom_line(aes(x = offset_time_as_date(t, start_date), y = c2vs, color = "Above Split", linetype = "Logistic Regression"), data = pi_bounds) +
            geom_line(aes(x = offset_time_as_date(t, start_date), y = wt, color = "WT", linetype = "Logistic Regression"), data = pi_bounds) +
            geom_line(aes(x = offset_time_as_date(t, start_date), y = nwt, color = "NWT", linetype = "Logistic Regression"), data = pi_bounds) +
            #geom_function(fun = function(t){(1 - predict(lr_output_bkpt, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response"))}, aes(color = "Below Split", linetype = "Logistic Regression")) +
            #geom_function(fun = function(t){predict(lr_output_bkpt, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response")}, aes(color = "Resistant", linetype = "Logistic Regression")) +
            #geom_function(fun = function(t){(1 - predict(lr_output_ecoff, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response"))}, aes(color = "WT", linetype = "Logistic Regression")) +
            #geom_function(fun = function(t){predict(lr_output_ecoff, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response")}, aes(color = "NWT", linetype = "Logistic Regression")) +
            scale_color_manual(breaks = c("Below Split", "Above Split",  "WT", "NWT"), values = c( "#DF4601", "#000000", "#fcbf07", "#0211a3"), name = "Other Prevalence") +  #+ guides(linetype = "none")
            scale_linetype_discrete(name = "Estimate Source")
        }
        visual_split_line = case_when(
          grepl("≤",visual_split) ~ visual_split %>% as.character() %>% parse_number() %>% log2,
          grepl("=",visual_split) ~ visual_split %>% as.character() %>% parse_number() %>% log2,
          grepl("<",visual_split) ~ visual_split %>% as.character() %>% parse_number() %>% log2 - 1,
          TRUE ~ visual_split %>% as.character() %>% parse_number() %>% log2
        )

        ecoff_line = case_when(
          grepl("≤", ecoff) ~ ecoff %>% as.character() %>% parse_number() %>% log2,
          grepl("=", ecoff) ~ ecoff %>% as.character() %>% parse_number() %>% log2,
          grepl("<", ecoff) ~ ecoff %>% as.character() %>% parse_number() %>% log2 - 1,
          TRUE ~ ecoff %>% as.character() %>% parse_number() %>% log2
        )

        if(visual_split_line == ecoff_line){
          visual_split_line = visual_split_line + 0.05
        }

        mean = mean +
          ggnewscale::new_scale_color() +
          geom_hline(aes(yintercept = visual_split_line, color = "Visual Split", linetype =  "Cutoff")) +
          geom_hline(aes(yintercept = ecoff_line, color = "ECOFF", linetype =  "Cutoff")) +

          scale_color_manual(breaks = c("Visual Split", "ECOFF"),
                             labels = c(TeX(paste0("Visual Split: ", visual_split,r'($\mu$)',"g/mL")), TeX(paste0("ECOFF: ", ecoff,r'($\mu$)',"g/mL"))),
                             values = c("#DF4601" , "#ffd700"), name = "Breakpoints and Cutoffs") +
          scale_linetype_manual(breaks=c("Fitted Model", "Cutoff"), values=c(1,2), guide = "none")  #+ guides(linetype = "none", color = "none")

      }else if(
        !is.na(ecoff) & is.na(visual_split) &
        !(is.na(s_breakpoint) & is.na(r_breakpoint))
      ){
        pi_bounds = pi_bounds %>%
          mutate(.by = t,
                 wt = 1 - predict(lr_output_ecoff, newdata = tibble(t = t), type = "response"),
                 nwt = predict(lr_output_ecoff, newdata = tibble(t = t), type = "response"),
                 susceptible = 1 - predict(lr_output_bkpt, newdata = tibble(t = t), type = "response"),
                 resistant = predict(lr_output_bkpt, newdata = tibble(t = t), type = "response") )


        if(!is.null(skip) && "ecoff" %in% skip){
          pi = pi +
            scale_color_manual(breaks = c("Component 1 Proportion", "Component 2 Proportion"), values = c("#e4190b", "#00999d"), labels = c(TeX(r'(Component 1 Prevalence: $\hat{\pi}_{1,t}$)'), TeX(r'(Component 2 Prevalence: $\hat{\pi}_{2,t}$)')), name = "Component Prevalence") +
            ggnewscale::new_scale_color() +
            geom_line(aes(x = offset_time_as_date(t, start_date), y = susceptible, color = "Susceptible", linetype = "Logistic Regression"), data = pi_bounds) +
            geom_line(aes(x = offset_time_as_date(t, start_date), y = resistant, color = "Resistant", linetype = "Logistic Regression"), data = pi_bounds) +
            #geom_function(fun = function(t){(1 - predict(lr_output_bkpt, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response"))}, aes(color = "Susceptible", linetype = "Logistic Regression")) +
            #geom_function(fun = function(t){predict(lr_output_bkpt, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response")}, aes(color = "Resistant", linetype = "Logistic Regression")) +
            #geom_function(fun = function(t){(1 - predict(lr_output_ecoff, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response"))}, aes(color = "WT", linetype = "Logistic Regression")) +
            #geom_function(fun = function(t){predict(lr_output_ecoff, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response")}, aes(color = "NWT", linetype = "Logistic Regression")) +
            scale_color_manual(breaks = c("Susceptible", "Resistant"), values = c( "#7CAE00", "#C77CFF"), name = "Other Prevalence") +  #+ guides(linetype = "none")
            scale_linetype_discrete(name = "Estimate Source")
        }else if(!is.null(skip) && "bkpts" %in% skip){
          pi = pi +
            scale_color_manual(breaks = c("Component 1 Proportion", "Component 2 Proportion"), values = c("#e4190b", "#00999d"), labels = c(TeX(r'(Component 1 Prevalence: $\hat{\pi}_{1,t}$)'), TeX(r'(Component 2 Prevalence: $\hat{\pi}_{2,t}$)')), name = "Component Prevalence") +
            ggnewscale::new_scale_color() +
            geom_line(aes(x = offset_time_as_date(t, start_date), y = wt, color = "WT", linetype = "Logistic Regression"), data = pi_bounds) +
            geom_line(aes(x = offset_time_as_date(t, start_date), y = nwt, color = "NWT", linetype = "Logistic Regression"), data = pi_bounds) +
            #geom_function(fun = function(t){(1 - predict(lr_output_bkpt, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response"))}, aes(color = "Susceptible", linetype = "Logistic Regression")) +
            #geom_function(fun = function(t){predict(lr_output_bkpt, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response")}, aes(color = "Resistant", linetype = "Logistic Regression")) +
            #geom_function(fun = function(t){(1 - predict(lr_output_ecoff, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response"))}, aes(color = "WT", linetype = "Logistic Regression")) +
            #geom_function(fun = function(t){predict(lr_output_ecoff, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response")}, aes(color = "NWT", linetype = "Logistic Regression")) +
            scale_color_manual(breaks = c( "WT", "NWT"), values = c("#fcbf07", "#0211a3"), name = "Other Prevalence") +  #+ guides(linetype = "none")
            scale_linetype_discrete(name = "Estimate Source")
        }else{

          pi = pi +
            scale_color_manual(breaks = c("Component 1 Proportion", "Component 2 Proportion"), values = c("#e4190b", "#00999d"), labels = c(TeX(r'(Component 1 Prevalence: $\hat{\pi}_{1,t}$)'), TeX(r'(Component 2 Prevalence: $\hat{\pi}_{2,t}$)')), name = "Component Prevalence") +
            ggnewscale::new_scale_color() +
            geom_line(aes(x = offset_time_as_date(t, start_date), y = susceptible, color = "Susceptible", linetype = "Logistic Regression"), data = pi_bounds) +
            geom_line(aes(x = offset_time_as_date(t, start_date), y = resistant, color = "Resistant", linetype = "Logistic Regression"), data = pi_bounds) +
            geom_line(aes(x = offset_time_as_date(t, start_date), y = wt, color = "WT", linetype = "Logistic Regression"), data = pi_bounds) +
            geom_line(aes(x = offset_time_as_date(t, start_date), y = nwt, color = "NWT", linetype = "Logistic Regression"), data = pi_bounds) +
            #geom_function(fun = function(t){(1 - predict(lr_output_bkpt, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response"))}, aes(color = "Susceptible", linetype = "Logistic Regression")) +
            #geom_function(fun = function(t){predict(lr_output_bkpt, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response")}, aes(color = "Resistant", linetype = "Logistic Regression")) +
            #geom_function(fun = function(t){(1 - predict(lr_output_ecoff, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response"))}, aes(color = "WT", linetype = "Logistic Regression")) +
            #geom_function(fun = function(t){predict(lr_output_ecoff, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response")}, aes(color = "NWT", linetype = "Logistic Regression")) +
            scale_color_manual(breaks = c("Susceptible", "Resistant",  "WT", "NWT"), values = c( "#7CAE00", "#C77CFF", "#fcbf07", "#0211a3"), name = "Other Prevalence") +  #+ guides(linetype = "none")
            scale_linetype_discrete(name = "Estimate Source")
        }


        s_line = case_when(
          grepl("≤",s_breakpoint) ~ s_breakpoint %>% as.character() %>% parse_number() %>% log2,
          grepl("=",s_breakpoint) ~ s_breakpoint %>% as.character() %>% parse_number() %>% log2,
          grepl("<",s_breakpoint) ~ s_breakpoint %>% as.character() %>% parse_number() %>% log2 - 1,
          TRUE ~ s_breakpoint %>% as.character() %>% parse_number() %>% log2
        )
        r_line = case_when(
          grepl("≥",r_breakpoint) ~ r_breakpoint %>% as.character() %>% parse_number() %>% log2 - 1,
          grepl("=",r_breakpoint) ~ r_breakpoint %>% as.character() %>% parse_number() %>% log2 - 1,
          grepl(">",r_breakpoint) ~ r_breakpoint %>% as.character() %>% parse_number() %>% log2,
          TRUE ~ r_breakpoint %>% as.character() %>% parse_number() %>% log2 - 1
        )

        ecoff_line = case_when(
          grepl("≤", ecoff) ~ ecoff %>% as.character() %>% parse_number() %>% log2,
          grepl("=", ecoff) ~ ecoff %>% as.character() %>% parse_number() %>% log2,
          grepl("<", ecoff) ~ ecoff %>% as.character() %>% parse_number() %>% log2 - 1,
          TRUE ~ ecoff %>% as.character() %>% parse_number() %>% log2
        )

        if(s_line == ecoff_line){
          s_line = s_line + 0.05
        }

        mean = mean +
          ggnewscale::new_scale_color() +
          geom_hline(aes(yintercept = s_line, color = "Susceptible Breakpoint", linetype =  "Breakpoint")) +
          geom_hline(aes(yintercept = r_line, color = "Resistant Breakpoint", linetype =  "Breakpoint")) +
          geom_hline(aes(yintercept = ecoff_line, color = "ECOFF", linetype =  "Cutoff")) +

          scale_color_manual(breaks = c("Susceptible Breakpoint", "Resistant Breakpoint", "ECOFF"),
                             labels = c(TeX(paste0("Susceptible Breakpoint: ", s_breakpoint,r'($\mu$)',"g/mL")), TeX(paste0("Resistant Breakpoint: ", r_breakpoint,r'($\mu$)',"g/mL")), TeX(paste0("ECOFF: ", ecoff,r'($\mu$)',"g/mL"))),
                             values = c("#7CAE00", "#C77CFF", "#ffd700"), name = "Breakpoints and Cutoffs") +
          scale_linetype_manual(breaks=c("Fitted Model","Breakpoint", "Cutoff"), values=c(1,5,2), guide = "none")  #+ guides(linetype = "none", color = "none")

      }else if(
        is.na(ecoff) & !is.na(visual_split) &
        !(is.na(s_breakpoint) & is.na(r_breakpoint))
      ){
        pi_bounds = pi_bounds %>%
          mutate(.by = t,
                 c1vs = 1 - predict(lr_output_visual_split, newdata = tibble(t = t), type = "response"),
                 c2vs = predict(lr_output_visual_split, newdata = tibble(t = t), type = "response"),
                 susceptible = 1 - predict(lr_output_bkpt, newdata = tibble(t = t), type = "response"),
                 resistant = predict(lr_output_bkpt, newdata = tibble(t = t), type = "response") )


        if(!is.null(skip) && "bkpts" %in% skip){
          pi = pi +
            scale_color_manual(breaks = c("Component 1 Proportion", "Component 2 Proportion"), values = c("#e4190b", "#00999d"), labels = c(TeX(r'(Component 1 Prevalence: $\hat{\pi}_{1,t}$)'), TeX(r'(Component 2 Prevalence: $\hat{\pi}_{2,t}$)')), name = "Component Prevalence") +
            ggnewscale::new_scale_color() +
            geom_line(aes(x = offset_time_as_date(t, start_date), y = c1vs, color = "Below Split", linetype = "Logistic Regression"), data = pi_bounds) +
            geom_line(aes(x = offset_time_as_date(t, start_date), y = c2vs, color = "Above Split", linetype = "Logistic Regression"), data = pi_bounds) +
            #geom_function(fun = function(t){(1 - predict(lr_output_bkpt, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response"))}, aes(color = "Susceptible", linetype = "Logistic Regression")) +
            #geom_function(fun = function(t){predict(lr_output_bkpt, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response")}, aes(color = "Resistant", linetype = "Logistic Regression")) +
            #geom_function(fun = function(t){(1 - predict(lr_output_visual_split, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response"))}, aes(color = "Below Split", linetype = "Logistic Regression")) +
            #geom_function(fun = function(t){predict(lr_output_visual_split, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response")}, aes(color = "Above Split", linetype = "Logistic Regression")) +
            scale_color_manual(breaks = c(  "Below Split", "Above Split"), values = c( "#DF4601", "#000000"), name = "Other Prevalence") +  #+ guides(linetype = "none")
            scale_linetype_discrete(name = "Estimate Source")
        }else{

          pi = pi +
            scale_color_manual(breaks = c("Component 1 Proportion", "Component 2 Proportion"), values = c("#e4190b", "#00999d"), labels = c(TeX(r'(Component 1 Prevalence: $\hat{\pi}_{1,t}$)'), TeX(r'(Component 2 Prevalence: $\hat{\pi}_{2,t}$)')), name = "Component Prevalence") +
            ggnewscale::new_scale_color() +
            geom_line(aes(x = offset_time_as_date(t, start_date), y = susceptible, color = "Susceptible", linetype = "Logistic Regression"), data = pi_bounds) +
            geom_line(aes(x = offset_time_as_date(t, start_date), y = resistant, color = "Resistant", linetype = "Logistic Regression"), data = pi_bounds) +
            geom_line(aes(x = offset_time_as_date(t, start_date), y = c1vs, color = "Below Split", linetype = "Logistic Regression"), data = pi_bounds) +
            geom_line(aes(x = offset_time_as_date(t, start_date), y = c2vs, color = "Above Split", linetype = "Logistic Regression"), data = pi_bounds) +
            #geom_function(fun = function(t){(1 - predict(lr_output_bkpt, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response"))}, aes(color = "Susceptible", linetype = "Logistic Regression")) +
            #geom_function(fun = function(t){predict(lr_output_bkpt, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response")}, aes(color = "Resistant", linetype = "Logistic Regression")) +
            #geom_function(fun = function(t){(1 - predict(lr_output_visual_split, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response"))}, aes(color = "Below Split", linetype = "Logistic Regression")) +
            #geom_function(fun = function(t){predict(lr_output_visual_split, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response")}, aes(color = "Above Split", linetype = "Logistic Regression")) +
            scale_color_manual(breaks = c("Susceptible", "Resistant",  "Below Split", "Above Split"), values = c( "#7CAE00", "#C77CFF", "#DF4601", "#000000"), name = "Other Prevalence") +  #+ guides(linetype = "none")
            scale_linetype_discrete(name = "Estimate Source")
        }


        s_line = case_when(
          grepl("≤",s_breakpoint) ~ s_breakpoint %>% as.character() %>% parse_number() %>% log2,
          grepl("=",s_breakpoint) ~ s_breakpoint %>% as.character() %>% parse_number() %>% log2,
          grepl("<",s_breakpoint) ~ s_breakpoint %>% as.character() %>% parse_number() %>% log2 - 1,
          TRUE ~ s_breakpoint %>% as.character() %>% parse_number() %>% log2
        )
        r_line = case_when(
          grepl("≥",r_breakpoint) ~ r_breakpoint %>% as.character() %>% parse_number() %>% log2 - 1,
          grepl("=",r_breakpoint) ~ r_breakpoint %>% as.character() %>% parse_number() %>% log2 - 1,
          grepl(">",r_breakpoint) ~ r_breakpoint %>% as.character() %>% parse_number() %>% log2,
          TRUE ~ r_breakpoint %>% as.character() %>% parse_number() %>% log2 - 1
        )

        visual_split_line = case_when(
          grepl("≤", visual_split) ~ visual_split %>% as.character() %>% parse_number() %>% log2,
          grepl("=", visual_split) ~ visual_split %>% as.character() %>% parse_number() %>% log2,
          grepl("<", visual_split) ~ visual_split %>% as.character() %>% parse_number() %>% log2 - 1,
          TRUE ~ visual_split %>% as.character() %>% parse_number() %>% log2
        )

        if(s_line == visual_split_line){
          s_line = s_line + 0.05
        }

        mean = mean +
          ggnewscale::new_scale_color() +
          geom_hline(aes(yintercept = s_line, color = "Susceptible Breakpoint", linetype =  "Breakpoint")) +
          geom_hline(aes(yintercept = r_line, color = "Resistant Breakpoint", linetype =  "Breakpoint")) +
          geom_hline(aes(yintercept = visual_split_line, color = "Visual Split", linetype =  "Cutoff")) +

          scale_color_manual(breaks = c("Susceptible Breakpoint", "Resistant Breakpoint", "Visual Split"),
                             labels = c(TeX(paste0("Susceptible Breakpoint: ", s_breakpoint,r'($\mu$)',"g/mL")), TeX(paste0("Resistant Breakpoint: ", r_breakpoint,r'($\mu$)',"g/mL")), TeX(paste0("Visual Split: ", visual_split,r'($\mu$)',"g/mL"))),
                             values = c("#7CAE00", "#C77CFF", "#DF4601"), name = "Breakpoints and Cutoffs") +
          scale_linetype_manual(breaks=c("Fitted Model","Breakpoint", "Cutoff"), values=c(1,5,2), guide = "none")  #+ guides(linetype = "none", color = "none")

      }else{
        pi_bounds = pi_bounds %>%
          mutate(.by = t,
                 wt = 1 - predict(lr_output_ecoff, newdata = tibble(t = t), type = "response"),
                 nwt = predict(lr_output_ecoff, newdata = tibble(t = t), type = "response"),
                 c1vs = 1 - predict(lr_output_visual_split, newdata = tibble(t = t), type = "response"),
                 c2vs = predict(lr_output_visual_split, newdata = tibble(t = t), type = "response"),
                 susceptible = 1 - predict(lr_output_bkpt, newdata = tibble(t = t), type = "response"),
                 resistant = predict(lr_output_bkpt, newdata = tibble(t = t), type = "response") )


        if(!is.null(skip) && "bkpts" %in% skip & "ecoff" %in% skip){
          pi = pi +
            scale_color_manual(breaks = c("Component 1 Proportion", "Component 2 Proportion"), values = c("#e4190b", "#00999d"), labels = c(TeX(r'(Component 1 Prevalence: $\hat{\pi}_{1,t}$)'), TeX(r'(Component 2 Prevalence: $\hat{\pi}_{2,t}$)')), name = "Component Prevalence") +
            ggnewscale::new_scale_color() +
            geom_line(aes(x = offset_time_as_date(t, start_date), y = wt, color = "Below Split", linetype = "Logistic Regression"), data = pi_bounds) +
            geom_line(aes(x = offset_time_as_date(t, start_date), y = nwt, color = "Above Split", linetype = "Logistic Regression"), data = pi_bounds) +
            #geom_function(fun = function(t){(1 - predict(lr_output_bkpt, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response"))}, aes(color = "Susceptible", linetype = "Logistic Regression")) +
            #geom_function(fun = function(t){predict(lr_output_bkpt, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response")}, aes(color = "Resistant", linetype = "Logistic Regression")) +
            #geom_function(fun = function(t){(1 - predict(lr_output_ecoff, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response"))}, aes(color = "WT", linetype = "Logistic Regression")) +
            #geom_function(fun = function(t){predict(lr_output_ecoff, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response")}, aes(color = "NWT", linetype = "Logistic Regression")) +
            scale_color_manual(breaks = c("Below Split", "Above Split"), values = c( "#DF4601", "#000000"), name = "Other Prevalence") +  #+ guides(linetype = "none")
            scale_linetype_discrete(name = "Estimate Source")
        }else if(!is.null(skip) && "bkpts" %in% skip){
          pi = pi +
            scale_color_manual(breaks = c("Component 1 Proportion", "Component 2 Proportion"), values = c("#e4190b", "#00999d"), labels = c(TeX(r'(Component 1 Prevalence: $\hat{\pi}_{1,t}$)'), TeX(r'(Component 2 Prevalence: $\hat{\pi}_{2,t}$)')), name = "Component Prevalence") +
            ggnewscale::new_scale_color() +
            geom_line(aes(x = offset_time_as_date(t, start_date), y = wt, color = "WT", linetype = "Logistic Regression"), data = pi_bounds) +
            geom_line(aes(x = offset_time_as_date(t, start_date), y = nwt, color = "NWT", linetype = "Logistic Regression"), data = pi_bounds) +
            geom_line(aes(x = offset_time_as_date(t, start_date), y = wt, color = "Below Split", linetype = "Logistic Regression"), data = pi_bounds) +
            geom_line(aes(x = offset_time_as_date(t, start_date), y = nwt, color = "Above Split", linetype = "Logistic Regression"), data = pi_bounds) +
            #geom_function(fun = function(t){(1 - predict(lr_output_bkpt, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response"))}, aes(color = "Susceptible", linetype = "Logistic Regression")) +
            #geom_function(fun = function(t){predict(lr_output_bkpt, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response")}, aes(color = "Resistant", linetype = "Logistic Regression")) +
            #geom_function(fun = function(t){(1 - predict(lr_output_ecoff, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response"))}, aes(color = "WT", linetype = "Logistic Regression")) +
            #geom_function(fun = function(t){predict(lr_output_ecoff, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response")}, aes(color = "NWT", linetype = "Logistic Regression")) +
            scale_color_manual(breaks = c("WT", "NWT", "Below Split", "Above Split"), values = c(  "#fcbf07", "#0211a3", "#DF4601", "#000000"), name = "Other Prevalence") +  #+ guides(linetype = "none")
            scale_linetype_discrete(name = "Estimate Source")
        }else if(!is.null(skip) && "ecoff" %in% skip){
          pi = pi +
            scale_color_manual(breaks = c("Component 1 Proportion", "Component 2 Proportion"), values = c("#e4190b", "#00999d"), labels = c(TeX(r'(Component 1 Prevalence: $\hat{\pi}_{1,t}$)'), TeX(r'(Component 2 Prevalence: $\hat{\pi}_{2,t}$)')), name = "Component Prevalence") +
            ggnewscale::new_scale_color() +
            geom_line(aes(x = offset_time_as_date(t, start_date), y = susceptible, color = "Susceptible", linetype = "Logistic Regression"), data = pi_bounds) +
            geom_line(aes(x = offset_time_as_date(t, start_date), y = resistant, color = "Resistant", linetype = "Logistic Regression"), data = pi_bounds) +
            geom_line(aes(x = offset_time_as_date(t, start_date), y = wt, color = "Below Split", linetype = "Logistic Regression"), data = pi_bounds) +
            geom_line(aes(x = offset_time_as_date(t, start_date), y = nwt, color = "Above Split", linetype = "Logistic Regression"), data = pi_bounds) +
            #geom_function(fun = function(t){(1 - predict(lr_output_bkpt, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response"))}, aes(color = "Susceptible", linetype = "Logistic Regression")) +
            #geom_function(fun = function(t){predict(lr_output_bkpt, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response")}, aes(color = "Resistant", linetype = "Logistic Regression")) +
            #geom_function(fun = function(t){(1 - predict(lr_output_ecoff, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response"))}, aes(color = "WT", linetype = "Logistic Regression")) +
            #geom_function(fun = function(t){predict(lr_output_ecoff, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response")}, aes(color = "NWT", linetype = "Logistic Regression")) +
            scale_color_manual(breaks = c("Susceptible", "Resistant","Below Split", "Above Split"), values = c( "#7CAE00", "#C77CFF", "#DF4601", "#000000"), name = "Other Prevalence") +  #+ guides(linetype = "none")
            scale_linetype_discrete(name = "Estimate Source")
        }else{

          pi = pi +
            scale_color_manual(breaks = c("Component 1 Proportion", "Component 2 Proportion"), values = c("#e4190b", "#00999d"), labels = c(TeX(r'(Component 1 Prevalence: $\hat{\pi}_{1,t}$)'), TeX(r'(Component 2 Prevalence: $\hat{\pi}_{2,t}$)')), name = "Component Prevalence") +
            ggnewscale::new_scale_color() +
            geom_line(aes(x = offset_time_as_date(t, start_date), y = susceptible, color = "Susceptible", linetype = "Logistic Regression"), data = pi_bounds) +
            geom_line(aes(x = offset_time_as_date(t, start_date), y = resistant, color = "Resistant", linetype = "Logistic Regression"), data = pi_bounds) +
            geom_line(aes(x = offset_time_as_date(t, start_date), y = wt, color = "WT", linetype = "Logistic Regression"), data = pi_bounds) +
            geom_line(aes(x = offset_time_as_date(t, start_date), y = nwt, color = "NWT", linetype = "Logistic Regression"), data = pi_bounds) +
            geom_line(aes(x = offset_time_as_date(t, start_date), y = wt, color = "Below Split", linetype = "Logistic Regression"), data = pi_bounds) +
            geom_line(aes(x = offset_time_as_date(t, start_date), y = nwt, color = "Above Split", linetype = "Logistic Regression"), data = pi_bounds) +
            #geom_function(fun = function(t){(1 - predict(lr_output_bkpt, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response"))}, aes(color = "Susceptible", linetype = "Logistic Regression")) +
            #geom_function(fun = function(t){predict(lr_output_bkpt, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response")}, aes(color = "Resistant", linetype = "Logistic Regression")) +
            #geom_function(fun = function(t){(1 - predict(lr_output_ecoff, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response"))}, aes(color = "WT", linetype = "Logistic Regression")) +
            #geom_function(fun = function(t){predict(lr_output_ecoff, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response")}, aes(color = "NWT", linetype = "Logistic Regression")) +
            scale_color_manual(breaks = c("Susceptible", "Resistant",  "WT", "NWT", "Below Split", "Above Split"), values = c( "#7CAE00", "#C77CFF", "#fcbf07", "#0211a3", "#DF4601", "#000000"), name = "Other Prevalence") +  #+ guides(linetype = "none")
            scale_linetype_discrete(name = "Estimate Source")

        }


        s_line = case_when(
          grepl("≤",s_breakpoint) ~ s_breakpoint %>% as.character() %>% parse_number() %>% log2,
          grepl("=",s_breakpoint) ~ s_breakpoint %>% as.character() %>% parse_number() %>% log2,
          grepl("<",s_breakpoint) ~ s_breakpoint %>% as.character() %>% parse_number() %>% log2 - 1,
          TRUE ~ s_breakpoint %>% as.character() %>% parse_number() %>% log2
        )
        r_line = case_when(
          grepl("≥",r_breakpoint) ~ r_breakpoint %>% as.character() %>% parse_number() %>% log2 - 1,
          grepl("=",r_breakpoint) ~ r_breakpoint %>% as.character() %>% parse_number() %>% log2 - 1,
          grepl(">",r_breakpoint) ~ r_breakpoint %>% as.character() %>% parse_number() %>% log2,
          TRUE ~ r_breakpoint %>% as.character() %>% parse_number() %>% log2 - 1
        )

        ecoff_line = case_when(
          grepl("≤", ecoff) ~ ecoff %>% as.character() %>% parse_number() %>% log2,
          grepl("=", ecoff) ~ ecoff %>% as.character() %>% parse_number() %>% log2,
          grepl("<", ecoff) ~ ecoff %>% as.character() %>% parse_number() %>% log2 - 1,
          TRUE ~ ecoff %>% as.character() %>% parse_number() %>% log2
        )

        visual_split_line = case_when(
          grepl("≤", visual_split) ~ visual_split %>% as.character() %>% parse_number() %>% log2,
          grepl("=", visual_split) ~ visual_split %>% as.character() %>% parse_number() %>% log2,
          grepl("<", visual_split) ~ visual_split %>% as.character() %>% parse_number() %>% log2 - 1,
          TRUE ~ visual_split %>% as.character() %>% parse_number() %>% log2
        )

        if(visual_split_line == ecoff_line){
          visual_split_line = visual_split_line + 0.05
        }

        if(s_line == ecoff_line){
          s_line = s_line + 0.05
        }
        if(s_line == visual_split_line){
          s_line = s_line + 0.05
        }

        mean = mean +
          ggnewscale::new_scale_color() +
          geom_hline(aes(yintercept = s_line, color = "Susceptible Breakpoint", linetype =  "Breakpoint")) +
          geom_hline(aes(yintercept = r_line, color = "Resistant Breakpoint", linetype =  "Breakpoint")) +
          geom_hline(aes(yintercept = ecoff_line, color = "ECOFF", linetype =  "Cutoff")) +
          geom_hline(aes(yintercept = visual_split_line, color = "Visual Split", linetype =  "Cutoff")) +

          scale_color_manual(breaks = c("Susceptible Breakpoint", "Resistant Breakpoint", "ECOFF", "Visual Split"),
                             labels = c(TeX(paste0("Susceptible Breakpoint: ", s_breakpoint,r'($\mu$)',"g/mL")), TeX(paste0("Resistant Breakpoint: ", r_breakpoint,r'($\mu$)',"g/mL")), TeX(paste0("ECOFF: ", ecoff,r'($\mu$)',"g/mL")), TeX(paste0("ECOFF: ", visual_split,r'($\mu$)',"g/mL"))),
                             values = c("#7CAE00", "#C77CFF", "#ffd700", "#DF4601"), name = "Breakpoints and Cutoffs") +
          scale_linetype_manual(breaks=c("Fitted Model","Breakpoint", "Cutoff", "Cutoff"), values=c(1,5,2,2), guide = "none")  #+ guides(linetype = "none", color = "none")

      }

      }else{
        mean = mean + guides(linetype = "none")

        pi_bounds = tibble(t = seq(0, max(output$possible_data$t), len = 300),
                           pi2 = predict(output$pi_model, newdata = data.frame(t = t), type = "response"),
                           pi1 = 1 - pi2,
                           pi_se = predict(output$pi_model, newdata = data.frame(t = t), se.fit = TRUE)$se.fit,
                           pi_1_lp = logit(1 - predict(output$pi_model, newdata = data.frame(t = t), type = "response")),
                           pi_2_lp = predict(output$pi_model, newdata = data.frame(t = t)),
                           pi_1_lb = inverse_logit(pi_1_lp - (1.96 * pi_se)),
                           pi_1_ub = inverse_logit(pi_1_lp + (1.96 * pi_se)),
                           pi_2_lb = inverse_logit(pi_2_lp - (1.96 * pi_se)),
                           pi_2_ub = inverse_logit(pi_2_lp + (1.96 * pi_se))
        )

        pi = df %>%
          offset_time_as_date_in_df(., start_date) %>%
          ggplot(aes(x = t)) +
          geom_line(aes(x = offset_time_as_date(t, start_date), y = pi1, linetype = "Fitted Model", color = "Component 1 Proportion"), data = pi_bounds) +
          geom_line(aes(x = offset_time_as_date(t, start_date), y = pi2, linetype = "Fitted Model", color = "Component 2 Proportion"), data = pi_bounds) +

          #geom_function(fun = function(t){(1 - predict(output$pi_model, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response"))}, aes(color = "Component 1 Proportion", linetype = "Fitted Model")) +
          #geom_function(fun = function(t){predict(output$pi_model, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response")}, aes(color = "Component 2 Proportion", linetype = "Fitted Model")) +
          scale_color_manual(breaks = c("Component 1 Proportion", "Component 2 Proportion"), values = c("#e4190b", "#00999d"), labels = c(TeX(r'(Component 1 Prevalence: $\hat{\pi}_{1,t}$)'), TeX(r'(Component 2 Prevalence: $\hat{\pi}_{2,t}$)')), name = "Component Prevalence") +
          ylim(0,1)  +
          xlab("Time") + ylab("Proportion") + theme_minimal() + guides(linetype = "none")
      }



    pi = pi +
      geom_ribbon(aes(ymin = pi_1_lb, ymax = pi_1_ub, x = offset_time_as_date(t, start_date), fill = "Component 1 Proportion"), data = pi_bounds, alpha = 0.2) +
      geom_ribbon(aes(ymin = pi_2_lb, ymax = pi_2_ub, x = offset_time_as_date(t, start_date), fill = "Component 2 Proportion"), data = pi_bounds, alpha = 0.2) +
      scale_fill_manual(breaks = c("Component 1 Proportion", "Component 2 Proportion"), values = c("#e4190b", "#00999d"), labels = c(TeX(r'(Component 1 Prevalence: $\hat{\pi}_{1,t}$)'), TeX(r'(Component 2 Prevalence: $\hat{\pi}_{2,t}$)')), name = "Component Prevalence")

if(!is.null(x_axis_t_breaks)){

  mean = mean + scale_x_continuous(breaks = x_axis_t_breaks %>% offset_time_as_date(., start_date = start_date), labels = x_axis_t_breaks %>% offset_time_as_date(., start_date = start_date) %>% year())
  pi = pi + scale_x_continuous(breaks = x_axis_t_breaks %>% offset_time_as_date(., start_date = start_date), labels = x_axis_t_breaks %>% offset_time_as_date(., start_date = start_date) %>% year())

}


    return(patchwork::wrap_plots(mean,pi, ncol = 1))

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
      mean <-
        df %>%
        offset_time_as_date_in_df(., start_date) %>%
        ggplot(aes(x = t)) +
        geom_segment(aes(x = t, xend = t, y = left_bound, yend = right_bound, color = "Observations"), data = (df %>% filter(cens == "int") %>% offset_time_as_date_in_df(., start_date)), alpha = 0.3) +
        geom_segment(aes(x = t, xend = t, y = right_bound, yend = left_bound, color = "Observations"), data = (df %>% filter(cens == "lc") %>% mutate(left_bound = plot_min) %>% offset_time_as_date_in_df(., start_date)), arrow = arrow(length = unit(0.03, "npc")), alpha = 0.3) +
        geom_segment(aes(x = t, xend = t, y = left_bound, yend = right_bound, color = "Observations"), data = (df %>% filter(cens == "rc") %>% mutate(right_bound = plot_max) %>% offset_time_as_date_in_df(., start_date)), arrow = arrow(length = unit(0.03, "npc")), alpha = 0.3) +
        geom_point(aes(x = t, y = left_bound,  color = "Observations"), data = df %>% filter(left_bound != -Inf) %>% offset_time_as_date_in_df(., start_date), alpha = 0.3) +
        geom_point(aes(x = t, y = right_bound,  color = "Observations"), data = df %>% filter(right_bound != Inf) %>% offset_time_as_date_in_df(., start_date), alpha = 0.3) +
        scale_colour_manual(values = c("Observations" = "#F8766D"), guide = "none") +
        ggnewscale::new_scale_color() +
#        geom_function(fun = function(t){predict(fitted_comp, newdata = data.frame(t = as_offset_time(x = t, start_date)))}, aes(color = "Component 1 Mu", linetype = "Fitted Model")) +
#        geom_function(fun = function(t){mu.se.brd.fms(t, z = 1.96)}, aes(color = "Component Mu", linetype = "Fitted Model SE"), size = 0.6, alpha = 0.6) +
#        geom_function(fun = function(t){mu.se.brd.fms(t, z = -1.96)}, aes(color = "Component Mu", linetype = "Fitted Model SE"), size = 0.6, alpha = 0.6) +
        geom_line(aes(x = offset_time_as_date(t, start_date), y = c1pred, color = "Component 1 Mu"), data = ci_data) +
        geom_ribbon(aes(ymin = c1pred_lb, ymax = c1pred_ub, x = offset_time_as_date(t, start_date), fill = "Component 1 Mu"), data = ci_data, alpha = 0.2) +
        geom_ribbon(aes(ymin = lwr, ymax = upr, x = t, fill = "Component 1 Mu"), data = sim_pi_survreg_boot(df, fit = fitted_comp, alpha = 0.05, nSims = 10000) %>% offset_time_as_date_in_df(., start_date), alpha = 0.15) +
        scale_color_manual(breaks = c("Component 1 Mu"), values = c("#e4190b"), labels = c(TeX(r'(Component 1 Mean: $\hat{\mu}_{1,t}$)')), name = "Component Mean") +
        scale_fill_manual(breaks = c("Component 1 Mu"), values = c("#e4190b"), labels = c(TeX(r'(Component 1 Mean: $\hat{\mu}_{1,t}$)')), name = "Component Mean") +
        #scale_linetype_manual(values = c("Fitted Model" = 1), guide = "none") +
        ggtitle(title) +
        xlab("Time") +
        ylab(bquote(log[2]~ MIC)) +
        ylim(plot_min - 1, plot_max + 1) +
        scale_y_continuous(breaks = scales::breaks_extended((plot_max - plot_min)/1.5)) +
        #scale_x_continuous(breaks = scales::breaks_extended(6)) +
        theme_minimal() +
        theme(legend.position = "bottom")

if(!is.na(ecoff) | (!is.na(s_breakpoint) & !is.na(r_breakpoint))){
      if(!is.na(ecoff) & (is.na(s_breakpoint) & is.na(r_breakpoint))){
        ecoff_line = case_when(
          grepl("≤", ecoff) ~ ecoff %>% as.character() %>% parse_number() %>% log2,
          grepl("=", ecoff) ~ ecoff %>% as.character() %>% parse_number() %>% log2,
          grepl("<", ecoff) ~ ecoff %>% as.character() %>% parse_number() %>% log2 - 1,
          TRUE ~ ecoff %>% as.character() %>% parse_number() %>% log2
        )
        mean = mean +
          ggnewscale::new_scale_color() +
          geom_hline(aes(yintercept = ecoff_line, color = "ECOFF", linetype =  "ECOFF")) +
          scale_color_manual(breaks = c("ECOFF"), values = c("#ffd700"), labels = c(TeX(paste0("ECOFF: ", ecoff,r'($\mu$)',"g/mL"))), name = "Breakpoints and Cutoffs") +
          scale_linetype_manual(breaks=c("ECOFF"), values=c(2), labels = c(TeX(paste0("ECOFF: ", ecoff,r'($\mu$)',"g/mL"))), name = "Breakpoints and Cutoffs")  #+ guides(linetype = "none", color = "none")

      }else if(is.na(ecoff) & (!is.na(s_breakpoint) & !is.na(r_breakpoint))){
        s_line = case_when(
          grepl("≤",s_breakpoint) ~ s_breakpoint %>% as.character() %>% parse_number() %>% log2,
          grepl("=",s_breakpoint) ~ s_breakpoint %>% as.character() %>% parse_number() %>% log2,
          grepl("<",s_breakpoint) ~ s_breakpoint %>% as.character() %>% parse_number() %>% log2 - 1,
          TRUE ~ s_breakpoint %>% as.character() %>% parse_number() %>% log2
        )
        r_line = case_when(
          grepl("≥",r_breakpoint) ~ r_breakpoint %>% as.character() %>% parse_number() %>% log2 - 1,
          grepl("=",r_breakpoint) ~ r_breakpoint %>% as.character() %>% parse_number() %>% log2 - 1,
          grepl(">",r_breakpoint) ~ r_breakpoint %>% as.character() %>% parse_number() %>% log2,
          TRUE ~ r_breakpoint %>% as.character() %>% parse_number() %>% log2 - 1
        )
        mean = mean +
          ggnewscale::new_scale_color() +
          geom_hline(aes(yintercept = s_line, color = "Susceptible Breakpoint", linetype =  "Susceptible Breakpoint")) +
          geom_hline(aes(yintercept = r_line, color = "Resistant Breakpoint", linetype =  "Resistant Breakpoint")) +
          scale_color_manual(breaks = c("Susceptible Breakpoint", "Resistant Breakpoint"), labels = c(TeX(paste0("Susceptible Breakpoint: ", s_breakpoint,r'($\mu$)',"g/mL")), TeX(paste0("Resistant Breakpoint: ", r_breakpoint,r'($\mu$)',"g/mL"))), values = c("#7CAE00", "#C77CFF"), name = "Breakpoints and Cutoffs") +
          scale_linetype_manual(breaks=c("Susceptible Breakpoint", "Resistant Breakpoint"), labels = c(TeX(paste0("Susceptible Breakpoint: ", s_breakpoint,r'($\mu$)',"g/mL")), TeX(paste0("Resistant Breakpoint: ", r_breakpoint,r'($\mu$)',"g/mL"))), values=c(5,5), name = "Breakpoints and Cutoffs")  #+ guides(linetype = "none", color = "none")
      }else{
        s_line = case_when(
          grepl("≤",s_breakpoint) ~ s_breakpoint %>% as.character() %>% parse_number() %>% log2,
          grepl("=",s_breakpoint) ~ s_breakpoint %>% as.character() %>% parse_number() %>% log2,
          grepl("<",s_breakpoint) ~ s_breakpoint %>% as.character() %>% parse_number() %>% log2 - 1,
          TRUE ~ s_breakpoint %>% as.character() %>% parse_number() %>% log2
        )
        r_line = case_when(
          grepl("≥",r_breakpoint) ~ r_breakpoint %>% as.character() %>% parse_number() %>% log2 - 1,
          grepl("=",r_breakpoint) ~ r_breakpoint %>% as.character() %>% parse_number() %>% log2 - 1,
          grepl(">",r_breakpoint) ~ r_breakpoint %>% as.character() %>% parse_number() %>% log2,
          TRUE ~ r_breakpoint %>% as.character() %>% parse_number() %>% log2 - 1
        )

        ecoff_line = case_when(
          grepl("≤", ecoff) ~ ecoff %>% as.character() %>% parse_number() %>% log2,
          grepl("=", ecoff) ~ ecoff %>% as.character() %>% parse_number() %>% log2,
          grepl("<", ecoff) ~ ecoff %>% as.character() %>% parse_number() %>% log2 - 1,
          TRUE ~ ecoff %>% as.character() %>% parse_number() %>% log2
        )


        if(s_line == ecoff_line){
          s_line = s_line + 0.05
        }

        mean = mean +
          ggnewscale::new_scale_color() +
          geom_hline(aes(yintercept = s_line, color = "Susceptible Breakpoint", linetype =  "Breakpoint")) +
          geom_hline(aes(yintercept = r_line, color = "Resistant Breakpoint", linetype =  "Breakpoint")) +
          geom_hline(aes(yintercept = ecoff_line, color = "ECOFF", linetype =  "Cutoff")) +

          scale_color_manual(breaks = c("Susceptible Breakpoint", "Resistant Breakpoint", "ECOFF"),
                             labels = c(TeX(paste0("Susceptible Breakpoint: ", s_breakpoint,r'($\mu$)',"g/mL")), TeX(paste0("Resistant Breakpoint: ", r_breakpoint,r'($\mu$)',"g/mL")), TeX(paste0("ECOFF: ", ecoff,r'($\mu$)',"g/mL"))),
                             values = c("#7CAE00", "#C77CFF", "#ffd700"), name = "Breakpoints and Cutoffs") +
          scale_linetype_manual(breaks=c("Susceptible Breakpoint", "Resistant Breakpoint", "ECOFF"),
                                labels = c(TeX(paste0("Susceptible Breakpoint: ", s_breakpoint,r'($\mu$)',"g/mL")), TeX(paste0("Resistant Breakpoint: ", r_breakpoint,r'($\mu$)',"g/mL")), TeX(paste0("ECOFF: ", ecoff,r'($\mu$)',"g/mL"))),
                                values=c(5,5,2), name = "Breakpoints and Cutoffs")  #+ guides(linetype = "none", color = "none")

      }

}

      if(!is.null(x_axis_t_breaks)){

        mean = mean + scale_x_continuous(breaks = x_axis_t_breaks %>% offset_time_as_date(., start_date = start_date), labels = x_axis_t_breaks %>% offset_time_as_date(., start_date = start_date) %>% year())

      }

      return(mean)

    }else if (output$ncomp == 2 & ncomp == 1){
      ci_data <- tibble(t = rep(seq(0, max(output$possible_data$t), len = 300), 2)) %>%
        mutate(
          c1pred = predict(fitted_comp, tibble(t), se = T)$fit,
          c1pred_se = predict(fitted_comp, tibble(t), se = T)$se.fit,
          c1pred_lb = c1pred - 1.96 * c1pred_se,
          c1pred_ub = c1pred + 1.96 * c1pred_se
        )
      if((results %>% filter(!dnc) %>% pull(c)) == 1){
      corresponding_color = "#e4190b"
      corresponding_label = TeX(r'(Component 1 Mean: $\hat{\mu}_{1,t}$)')
      }else{ #comp1
      corresponding_color = "#00999d"
      corresponding_label = TeX(r'(Component 2 Mean: $\hat{\mu}_{2,t}$)')
      }#comp2


      mean <- df %>%
        offset_time_as_date_in_df(., start_date) %>%
        ggplot(aes(x = t)) +
        scale_colour_gradient2(high = "#00BFC4", low = "#F8766D", mid = "green", midpoint = 0.5, name = "P(C=2|y,t)") +
        geom_segment(aes(x = t, xend = t, y = left_bound, yend = right_bound, color = `P(C=c|y,t)`), data = (df %>% filter(cens == "int" & c == "2") %>% offset_time_as_date_in_df(., start_date)), alpha = 0.2) +
        geom_segment(aes(x = t, xend = t, y = right_bound, yend = left_bound, color = `P(C=c|y,t)`), data = (df %>% filter(cens == "lc" & c == "2") %>% mutate(left_bound = plot_min) %>% offset_time_as_date_in_df(., start_date)), arrow = arrow(length = unit(0.03, "npc")), alpha = 0.2) +
        geom_segment(aes(x = t, xend = t, y = left_bound, yend = right_bound, color = `P(C=c|y,t)`), data = (df %>% filter(cens == "rc"& c == "2") %>% mutate(right_bound = plot_max) %>% offset_time_as_date_in_df(., start_date)), arrow = arrow(length = unit(0.03, "npc")), alpha = 0.2) +
        geom_point(aes(x = t, y = left_bound,  color = `P(C=c|y,t)`), data = (df %>% filter(left_bound != -Inf & c == "2") %>% offset_time_as_date_in_df(., start_date)), alpha = 0.2) +
        geom_point(aes(x = t, y = right_bound,  color = `P(C=c|y,t)`), data = (df %>% filter(right_bound != Inf & c == "2") %>% offset_time_as_date_in_df(., start_date)), alpha = 0.2) +
        ggnewscale::new_scale_color() +
        #geom_function(fun = function(t){predict(fitted_comp, newdata = data.frame(t = as_offset_time(x = t, start_date)))}, aes(color = "Component Mu", linetype = "Fitted Model")) +
        geom_line(aes(x = offset_time_as_date(t, start_date), y = c1pred, color = "Component Mu", linetype = "Fitted Model"), data = ci_data) +
        geom_ribbon(aes(ymin = c1pred_lb, ymax = c1pred_ub, x = offset_time_as_date(t, start_date), fill = "Component Mu"), data = ci_data, alpha = 0.2) +
        geom_ribbon(aes(ymin = lwr, ymax = upr, x = t, fill = "Component Mu"), data = sim_pi_survreg_boot(df, fit = fitted_comp, alpha = 0.05, nSims = 10000) %>% offset_time_as_date_in_df(., start_date), alpha = 0.15) +
        scale_color_manual(breaks = c("Component Mu"), values = c(corresponding_color), labels = c(corresponding_label), name = "Component Mean") +
        scale_fill_manual(breaks = c("Component Mu"), values = c(corresponding_color), labels = c(corresponding_label), name = "Component Mean") +


        #scale_colour_gradientn(colours = c("purple", "orange")) +
        #ylim(plot_min - 0.5, plot_max + 0.5) +
        ggtitle(title) +
        xlab("Time") +
        ylab(bquote(log[2]~ MIC)) +
        ylim(plot_min - 1, plot_max + 1) +
        scale_y_continuous(breaks = scales::breaks_extended((plot_max - plot_min)/1.5)) +
        #scale_x_continuous(breaks = scales::breaks_extended(6)) +
        theme_minimal()

      # pi <- df %>%
      #   offset_time_as_date_in_df(., start_date) %>%
      #   ggplot(aes(x = t)) +
      #   geom_function(fun = function(t){(1 - predict(output$pi_model, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response"))}, aes(color = "Component 1 Proportion", linetype = "Fitted Model")) +
      #   geom_function(fun = function(t){predict(output$pi_model, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response")}, aes(color = "Component 2 Proportion", linetype = "Fitted Model")) +
      #   scale_color_manual(breaks = c("Component 1 Proportion", "Component 2 Proportion"), values = c("#e4190b", "#00BFC4"), name = "Component Prevalence") +
      #   ylim(0,1)  +
      #   xlab("Time") + ylab("Proportion") + theme_minimal()


        if(add_log_reg && (!is.na(ecoff) | (!is.na(s_breakpoint) & !is.na(r_breakpoint)) | !is.na(visual_split))){
          if(!is.na(s_breakpoint) & !is.na(r_breakpoint)){
            lr_output_bkpt = log_reg(output$possible_data, split_by = "r_breakpoint", data_type = "possible_data", drug = NULL, date_col = "t", date_type = "decimal", first_year = NULL, s_breakpoint = s_breakpoint, r_breakpoint = r_breakpoint)
          }
          if(!is.na(ecoff)){
            lr_output_ecoff = log_reg(output$possible_data, split_by = "ecoff", data_type = "possible_data", drug = NULL, date_col = "t", date_type = "decimal", first_year = NULL, ecoff = ecoff)
          }
          if(!is.na(visual_split)){
            lr_output_visual_split = log_reg(output$possible_data, split_by = "visual_split", data_type = "possible_data", drug = NULL, date_col = "t", date_type = "decimal", first_year = NULL, visual_split = visual_split)
          }

          pi_bounds = tibble(t = seq(0, max(output$possible_data$t), len = 300),
                             pi2 = predict(output$pi_model, newdata = data.frame(t = t), type = "response"),
                             pi1 = 1 - pi2,
                             pi_se = predict(output$pi_model, newdata = data.frame(t = t), se.fit = TRUE)$se.fit,
                             pi_1_lp = logit(1 - predict(output$pi_model, newdata = data.frame(t = t), type = "response")),
                             pi_2_lp = predict(output$pi_model, newdata = data.frame(t = t)),
                             pi_1_lb = inverse_logit(pi_1_lp - (1.96 * pi_se)),
                             pi_1_ub = inverse_logit(pi_1_lp + (1.96 * pi_se)),
                             pi_2_lb = inverse_logit(pi_2_lp - (1.96 * pi_se)),
                             pi_2_ub = inverse_logit(pi_2_lp + (1.96 * pi_se))
          )

          pi = df %>%
            offset_time_as_date_in_df(., start_date) %>%
            ggplot(aes(x = t)) +
            geom_line(aes(x = offset_time_as_date(t, start_date), y = pi1, color = "Component 1 Proportion", linetype = "Fitted Model"), data = pi_bounds) +
            geom_line(aes(x = offset_time_as_date(t, start_date), y = pi2, color = "Component 2 Proportion", linetype = "Fitted Model"), data = pi_bounds) +
            #geom_function(fun = function(t){(1 - predict(output$pi_model, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response"))}, aes(color = "Component 1 Proportion", linetype = "Fitted Model")) +
            #geom_function(fun = function(t){predict(output$pi_model, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response")}, aes(color = "Component 2 Proportion", linetype = "Fitted Model")) +
            #scale_color_manual(breaks = c("Component 1 Proportion", "Component 2 Proportion"), values = c("#e4190b", "#00BFC4"), name = "Component Prevalence") +
            ylim(0,1)  +
            xlab("Time") + ylab("Proportion") + theme_minimal()

          if(is.na(ecoff) & is.na(visual_split) &
             (!is.na(s_breakpoint) & !is.na(r_breakpoint))
          ){
            pi_bounds = pi_bounds %>%
              mutate(.by = t,
                     susceptible = 1 - predict(lr_output_bkpt, newdata = tibble(t = t), type = "response"),
                     resistant = predict(lr_output_bkpt, newdata = tibble(t = t), type = "response") )

            pi = pi +
              scale_color_manual(breaks = c("Component 1 Proportion", "Component 2 Proportion"), values = c("#e4190b", "#00999d"), labels = c(TeX(r'(Component 1 Prevalence: $\hat{\pi}_{1,t}$)'), TeX(r'(Component 2 Prevalence: $\hat{\pi}_{2,t}$)')), name = "Component Prevalence") +
              ggnewscale::new_scale_color() +
              geom_line(aes(x = offset_time_as_date(t, start_date), y = susceptible, color = "Susceptible", linetype = "Logistic Regression"), data = pi_bounds) +
              geom_line(aes(x = offset_time_as_date(t, start_date), y = resistant, color = "Resistant", linetype = "Logistic Regression"), data = pi_bounds) +
              #geom_function(fun = function(t){(1 - predict(lr_output_bkpt, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response"))}, aes(color = "Susceptible", linetype = "Logistic Regression")) +
              #geom_function(fun = function(t){predict(lr_output_bkpt, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response")}, aes(color = "Resistant", linetype = "Logistic Regression")) +
              scale_color_manual(breaks = c("Susceptible", "Resistant"), values = c("#7CAE00", "#C77CFF"), name = "Other Prevalence") + #+ guides(linetype = "none")
              scale_linetype_discrete(name = "Estimate Source")

            s_line = case_when(
              grepl("≤",s_breakpoint) ~ s_breakpoint %>% as.character() %>% parse_number() %>% log2,
              grepl("=",s_breakpoint) ~ s_breakpoint %>% as.character() %>% parse_number() %>% log2,
              grepl("<",s_breakpoint) ~ s_breakpoint %>% as.character() %>% parse_number() %>% log2 - 1,
              TRUE ~ s_breakpoint %>% as.character() %>% parse_number() %>% log2
            )
            r_line = case_when(
              grepl("≥",r_breakpoint) ~ r_breakpoint %>% as.character() %>% parse_number() %>% log2 - 1,
              grepl("=",r_breakpoint) ~ r_breakpoint %>% as.character() %>% parse_number() %>% log2 - 1,
              grepl(">",r_breakpoint) ~ r_breakpoint %>% as.character() %>% parse_number() %>% log2,
              TRUE ~ r_breakpoint %>% as.character() %>% parse_number() %>% log2 - 1
            )

            mean = mean +
              ggnewscale::new_scale_color() +
              geom_hline(aes(yintercept = s_line, color = "Susceptible Breakpoint", linetype =  "Breakpoint")) +
              geom_hline(aes(yintercept = r_line, color = "Resistant Breakpoint", linetype =  "Breakpoint")) +
              scale_color_manual(breaks = c("Susceptible Breakpoint", "Resistant Breakpoint"), labels = c(TeX(paste0("Susceptible Breakpoint: ", s_breakpoint,r'($\mu$)',"g/mL")), TeX(paste0("Resistant Breakpoint: ", r_breakpoint,r'($\mu$)',"g/mL"))), values = c("#7CAE00", "#C77CFF"), name = "Breakpoints and Cutoffs") +
              scale_linetype_manual(breaks=c("Fitted Model","Breakpoint"), values=c(1,5), guide = "none")  #+ guides(linetype = "none", color = "none")
          }else if(
            !is.na(ecoff) & is.na(visual_split) &
            (is.na(s_breakpoint) & is.na(r_breakpoint))
          ){
            pi_bounds = pi_bounds %>%
              mutate(.by = t,
                     wt = 1 - predict(lr_output_ecoff, newdata = tibble(t = t), type = "response"),
                     nwt = predict(lr_output_ecoff, newdata = tibble(t = t), type = "response") )


            pi = pi +
              scale_color_manual(breaks = c("Component 1 Proportion", "Component 2 Proportion"), values = c("#e4190b", "#00999d"), labels = c(TeX(r'(Component 1 Prevalence: $\hat{\pi}_{1,t}$)'), TeX(r'(Component 2 Prevalence: $\hat{\pi}_{2,t}$)')), name = "Component Prevalence") +
              ggnewscale::new_scale_color() +
              geom_line(aes(x = offset_time_as_date(t, start_date), y = wt, color = "WT", linetype = "Logistic Regression"), data = pi_bounds) +
              geom_line(aes(x = offset_time_as_date(t, start_date), y = nwt, color = "NWT", linetype = "Logistic Regression"), data = pi_bounds) +

              #geom_function(fun = function(t){(1 - predict(lr_output_ecoff, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response"))}, aes(color = "WT", linetype = "Logistic Regression")) +
              #geom_function(fun = function(t){predict(lr_output_ecoff, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response")}, aes(color = "NWT", linetype = "Logistic Regression")) +
              scale_color_manual(breaks = c("WT", "NWT"), values = c( "#fcbf07", "#0211a3"), name = "Other Prevalence")  + #guides(linetype = "none")
              scale_linetype_discrete(name = "Estimate Source")

            ecoff_line = case_when(
              grepl("≤", ecoff) ~ ecoff %>% as.character() %>% parse_number() %>% log2,
              grepl("=", ecoff) ~ ecoff %>% as.character() %>% parse_number() %>% log2,
              grepl("<", ecoff) ~ ecoff %>% as.character() %>% parse_number() %>% log2 - 1,
              TRUE ~ ecoff %>% as.character() %>% parse_number() %>% log2
            )

            mean = mean +
              ggnewscale::new_scale_color() +
              geom_hline(aes(yintercept = ecoff_line, color = "ECOFF", linetype =  "Cutoff")) +
              scale_color_manual(breaks = c("ECOFF"), values = c("#ffd700"), labels = c(TeX(paste0("ECOFF: ", ecoff,r'($\mu$)',"g/mL"))), name = "Breakpoints and Cutoffs") +
              scale_linetype_manual(breaks=c("Fitted Model","Cutoff"), values=c(1,2), guide = "none")  #+ guides(linetype = "none", color = "none")
          }else if(
            is.na(ecoff) & !is.na(visual_split) &
            (is.na(s_breakpoint) & is.na(r_breakpoint))
          ){
            pi_bounds = pi_bounds %>%
              mutate(.by = t,
                     c1vs = 1 - predict(lr_output_visual_split, newdata = tibble(t = t), type = "response"),
                     c2vs = predict(lr_output_visual_split, newdata = tibble(t = t), type = "response") )


            pi = pi +
              scale_color_manual(breaks = c("Component 1 Proportion", "Component 2 Proportion"), values = c("#e4190b", "#00999d"), labels = c(TeX(r'(Component 1 Prevalence: $\hat{\pi}_{1,t}$)'), TeX(r'(Component 2 Prevalence: $\hat{\pi}_{2,t}$)')), name = "Component Prevalence") +
              ggnewscale::new_scale_color() +
              geom_line(aes(x = offset_time_as_date(t, start_date), y = c1vs, color = "Below Split", linetype = "Logistic Regression"), data = pi_bounds) +
              geom_line(aes(x = offset_time_as_date(t, start_date), y = c2vs, color = "Above Split", linetype = "Logistic Regression"), data = pi_bounds) +

              #geom_function(fun = function(t){(1 - predict(lr_output_ecoff, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response"))}, aes(color = "WT", linetype = "Logistic Regression")) +
              #geom_function(fun = function(t){predict(lr_output_ecoff, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response")}, aes(color = "NWT", linetype = "Logistic Regression")) +
              scale_color_manual(breaks = c("Below Split", "Above Split"), values = c( "#DF4601", "#000000"), name = "Other Prevalence")  + #guides(linetype = "none")
              scale_linetype_discrete(name = "Estimate Source")

            visual_split_line = case_when(
              grepl("≤", visual_split) ~ visual_split %>% as.character() %>% parse_number() %>% log2,
              grepl("=", visual_split) ~ visual_split %>% as.character() %>% parse_number() %>% log2,
              grepl("<", visual_split) ~ visual_split %>% as.character() %>% parse_number() %>% log2 - 1,
              TRUE ~ visual_split %>% as.character() %>% parse_number() %>% log2
            )

            mean = mean +
              ggnewscale::new_scale_color() +
              geom_hline(aes(yintercept = visual_split_line, color = "Visual Split", linetype =  "Cutoff")) +
              scale_color_manual(breaks = c("Visual Split"), values = c("#DF4601"), labels = c(TeX(paste0("Visual Split: ", visual_split,r'($\mu$)',"g/mL"))), name = "Breakpoints and Cutoffs") +
              scale_linetype_manual(breaks=c("Fitted Model","Cutoff"), values=c(1,2), guide = "none")  #+ guides(linetype = "none", color = "none")

          }else if(
            !is.na(ecoff) & !is.na(visual_split) &
            (is.na(s_breakpoint) & is.na(r_breakpoint))
          ){
            pi_bounds = pi_bounds %>%
              mutate(.by = t,
                     wt = 1 - predict(lr_output_ecoff, newdata = tibble(t = t), type = "response"),
                     nwt = predict(lr_output_ecoff, newdata = tibble(t = t), type = "response"),
                     c1vs = 1 - predict(lr_output_visual_split, newdata = tibble(t = t), type = "response"),
                     c2vs = predict(lr_output_visual_split, newdata = tibble(t = t), type = "response") )



            if(!is.null(skip) && "ecoff" %in% skip){
              pi = pi +
                scale_color_manual(breaks = c("Component 1 Proportion", "Component 2 Proportion"), values = c("#e4190b", "#00999d"), labels = c(TeX(r'(Component 1 Prevalence: $\hat{\pi}_{1,t}$)'), TeX(r'(Component 2 Prevalence: $\hat{\pi}_{2,t}$)')), name = "Component Prevalence") +
                ggnewscale::new_scale_color() +
                geom_line(aes(x = offset_time_as_date(t, start_date), y = c1vs, color = "Below Split", linetype = "Logistic Regression"), data = pi_bounds) +
                geom_line(aes(x = offset_time_as_date(t, start_date), y = c2vs, color = "Above Split", linetype = "Logistic Regression"), data = pi_bounds) +
                #geom_function(fun = function(t){(1 - predict(lr_output_bkpt, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response"))}, aes(color = "Below Split", linetype = "Logistic Regression")) +
                #geom_function(fun = function(t){predict(lr_output_bkpt, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response")}, aes(color = "Resistant", linetype = "Logistic Regression")) +
                #geom_function(fun = function(t){(1 - predict(lr_output_ecoff, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response"))}, aes(color = "WT", linetype = "Logistic Regression")) +
                #geom_function(fun = function(t){predict(lr_output_ecoff, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response")}, aes(color = "NWT", linetype = "Logistic Regression")) +
                scale_color_manual(breaks = c("Below Split", "Above Split"), values = c( "#DF4601", "#000000"), name = "Other Prevalence") +  #+ guides(linetype = "none")
                scale_linetype_discrete(name = "Estimate Source")
            }else{
              pi = pi +
                scale_color_manual(breaks = c("Component 1 Proportion", "Component 2 Proportion"), values = c("#e4190b", "#00999d"), labels = c(TeX(r'(Component 1 Prevalence: $\hat{\pi}_{1,t}$)'), TeX(r'(Component 2 Prevalence: $\hat{\pi}_{2,t}$)')), name = "Component Prevalence") +
                ggnewscale::new_scale_color() +
                geom_line(aes(x = offset_time_as_date(t, start_date), y = c1vs, color = "Below Split", linetype = "Logistic Regression"), data = pi_bounds) +
                geom_line(aes(x = offset_time_as_date(t, start_date), y = c2vs, color = "Above Split", linetype = "Logistic Regression"), data = pi_bounds) +
                geom_line(aes(x = offset_time_as_date(t, start_date), y = wt, color = "WT", linetype = "Logistic Regression"), data = pi_bounds) +
                geom_line(aes(x = offset_time_as_date(t, start_date), y = nwt, color = "NWT", linetype = "Logistic Regression"), data = pi_bounds) +
                #geom_function(fun = function(t){(1 - predict(lr_output_bkpt, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response"))}, aes(color = "Below Split", linetype = "Logistic Regression")) +
                #geom_function(fun = function(t){predict(lr_output_bkpt, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response")}, aes(color = "Resistant", linetype = "Logistic Regression")) +
                #geom_function(fun = function(t){(1 - predict(lr_output_ecoff, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response"))}, aes(color = "WT", linetype = "Logistic Regression")) +
                #geom_function(fun = function(t){predict(lr_output_ecoff, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response")}, aes(color = "NWT", linetype = "Logistic Regression")) +
                scale_color_manual(breaks = c("Below Split", "Above Split",  "WT", "NWT"), values = c( "#DF4601", "#000000", "#fcbf07", "#0211a3"), name = "Other Prevalence") +  #+ guides(linetype = "none")
                scale_linetype_discrete(name = "Estimate Source")
            }
            visual_split_line = case_when(
              grepl("≤",visual_split) ~ visual_split %>% as.character() %>% parse_number() %>% log2,
              grepl("=",visual_split) ~ visual_split %>% as.character() %>% parse_number() %>% log2,
              grepl("<",visual_split) ~ visual_split %>% as.character() %>% parse_number() %>% log2 - 1,
              TRUE ~ visual_split %>% as.character() %>% parse_number() %>% log2
            )

            ecoff_line = case_when(
              grepl("≤", ecoff) ~ ecoff %>% as.character() %>% parse_number() %>% log2,
              grepl("=", ecoff) ~ ecoff %>% as.character() %>% parse_number() %>% log2,
              grepl("<", ecoff) ~ ecoff %>% as.character() %>% parse_number() %>% log2 - 1,
              TRUE ~ ecoff %>% as.character() %>% parse_number() %>% log2
            )

            if(visual_split_line == ecoff_line){
              visual_split_line = visual_split_line + 0.05
            }

            mean = mean +
              ggnewscale::new_scale_color() +
              geom_hline(aes(yintercept = visual_split_line, color = "Visual Split", linetype =  "Cutoff")) +
              geom_hline(aes(yintercept = ecoff_line, color = "ECOFF", linetype =  "Cutoff")) +

              scale_color_manual(breaks = c("Visual Split", "ECOFF"),
                                 labels = c(TeX(paste0("Visual Split: ", visual_split,r'($\mu$)',"g/mL")), TeX(paste0("ECOFF: ", ecoff,r'($\mu$)',"g/mL"))),
                                 values = c("#DF4601" , "#ffd700"), name = "Breakpoints and Cutoffs") +
              scale_linetype_manual(breaks=c("Fitted Model", "Cutoff"), values=c(1,2), guide = "none")  #+ guides(linetype = "none", color = "none")

          }else if(
            !is.na(ecoff) & is.na(visual_split) &
            !(is.na(s_breakpoint) & is.na(r_breakpoint))
          ){
            pi_bounds = pi_bounds %>%
              mutate(.by = t,
                     wt = 1 - predict(lr_output_ecoff, newdata = tibble(t = t), type = "response"),
                     nwt = predict(lr_output_ecoff, newdata = tibble(t = t), type = "response"),
                     susceptible = 1 - predict(lr_output_bkpt, newdata = tibble(t = t), type = "response"),
                     resistant = predict(lr_output_bkpt, newdata = tibble(t = t), type = "response") )


            if(!is.null(skip) && "ecoff" %in% skip){
              pi = pi +
                scale_color_manual(breaks = c("Component 1 Proportion", "Component 2 Proportion"), values = c("#e4190b", "#00999d"), labels = c(TeX(r'(Component 1 Prevalence: $\hat{\pi}_{1,t}$)'), TeX(r'(Component 2 Prevalence: $\hat{\pi}_{2,t}$)')), name = "Component Prevalence") +
                ggnewscale::new_scale_color() +
                geom_line(aes(x = offset_time_as_date(t, start_date), y = susceptible, color = "Susceptible", linetype = "Logistic Regression"), data = pi_bounds) +
                geom_line(aes(x = offset_time_as_date(t, start_date), y = resistant, color = "Resistant", linetype = "Logistic Regression"), data = pi_bounds) +
                #geom_function(fun = function(t){(1 - predict(lr_output_bkpt, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response"))}, aes(color = "Susceptible", linetype = "Logistic Regression")) +
                #geom_function(fun = function(t){predict(lr_output_bkpt, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response")}, aes(color = "Resistant", linetype = "Logistic Regression")) +
                #geom_function(fun = function(t){(1 - predict(lr_output_ecoff, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response"))}, aes(color = "WT", linetype = "Logistic Regression")) +
                #geom_function(fun = function(t){predict(lr_output_ecoff, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response")}, aes(color = "NWT", linetype = "Logistic Regression")) +
                scale_color_manual(breaks = c("Susceptible", "Resistant"), values = c( "#7CAE00", "#C77CFF"), name = "Other Prevalence") +  #+ guides(linetype = "none")
                scale_linetype_discrete(name = "Estimate Source")
            }else if(!is.null(skip) && "bkpts" %in% skip){
              pi = pi +
                scale_color_manual(breaks = c("Component 1 Proportion", "Component 2 Proportion"), values = c("#e4190b", "#00999d"), labels = c(TeX(r'(Component 1 Prevalence: $\hat{\pi}_{1,t}$)'), TeX(r'(Component 2 Prevalence: $\hat{\pi}_{2,t}$)')), name = "Component Prevalence") +
                ggnewscale::new_scale_color() +
                geom_line(aes(x = offset_time_as_date(t, start_date), y = wt, color = "WT", linetype = "Logistic Regression"), data = pi_bounds) +
                geom_line(aes(x = offset_time_as_date(t, start_date), y = nwt, color = "NWT", linetype = "Logistic Regression"), data = pi_bounds) +
                #geom_function(fun = function(t){(1 - predict(lr_output_bkpt, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response"))}, aes(color = "Susceptible", linetype = "Logistic Regression")) +
                #geom_function(fun = function(t){predict(lr_output_bkpt, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response")}, aes(color = "Resistant", linetype = "Logistic Regression")) +
                #geom_function(fun = function(t){(1 - predict(lr_output_ecoff, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response"))}, aes(color = "WT", linetype = "Logistic Regression")) +
                #geom_function(fun = function(t){predict(lr_output_ecoff, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response")}, aes(color = "NWT", linetype = "Logistic Regression")) +
                scale_color_manual(breaks = c( "WT", "NWT"), values = c("#fcbf07", "#0211a3"), name = "Other Prevalence") +  #+ guides(linetype = "none")
                scale_linetype_discrete(name = "Estimate Source")
            }else{

              pi = pi +
                scale_color_manual(breaks = c("Component 1 Proportion", "Component 2 Proportion"), values = c("#e4190b", "#00999d"), labels = c(TeX(r'(Component 1 Prevalence: $\hat{\pi}_{1,t}$)'), TeX(r'(Component 2 Prevalence: $\hat{\pi}_{2,t}$)')), name = "Component Prevalence") +
                ggnewscale::new_scale_color() +
                geom_line(aes(x = offset_time_as_date(t, start_date), y = susceptible, color = "Susceptible", linetype = "Logistic Regression"), data = pi_bounds) +
                geom_line(aes(x = offset_time_as_date(t, start_date), y = resistant, color = "Resistant", linetype = "Logistic Regression"), data = pi_bounds) +
                geom_line(aes(x = offset_time_as_date(t, start_date), y = wt, color = "WT", linetype = "Logistic Regression"), data = pi_bounds) +
                geom_line(aes(x = offset_time_as_date(t, start_date), y = nwt, color = "NWT", linetype = "Logistic Regression"), data = pi_bounds) +
                #geom_function(fun = function(t){(1 - predict(lr_output_bkpt, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response"))}, aes(color = "Susceptible", linetype = "Logistic Regression")) +
                #geom_function(fun = function(t){predict(lr_output_bkpt, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response")}, aes(color = "Resistant", linetype = "Logistic Regression")) +
                #geom_function(fun = function(t){(1 - predict(lr_output_ecoff, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response"))}, aes(color = "WT", linetype = "Logistic Regression")) +
                #geom_function(fun = function(t){predict(lr_output_ecoff, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response")}, aes(color = "NWT", linetype = "Logistic Regression")) +
                scale_color_manual(breaks = c("Susceptible", "Resistant",  "WT", "NWT"), values = c( "#7CAE00", "#C77CFF", "#fcbf07", "#0211a3"), name = "Other Prevalence") +  #+ guides(linetype = "none")
                scale_linetype_discrete(name = "Estimate Source")
            }


            s_line = case_when(
              grepl("≤",s_breakpoint) ~ s_breakpoint %>% as.character() %>% parse_number() %>% log2,
              grepl("=",s_breakpoint) ~ s_breakpoint %>% as.character() %>% parse_number() %>% log2,
              grepl("<",s_breakpoint) ~ s_breakpoint %>% as.character() %>% parse_number() %>% log2 - 1,
              TRUE ~ s_breakpoint %>% as.character() %>% parse_number() %>% log2
            )
            r_line = case_when(
              grepl("≥",r_breakpoint) ~ r_breakpoint %>% as.character() %>% parse_number() %>% log2 - 1,
              grepl("=",r_breakpoint) ~ r_breakpoint %>% as.character() %>% parse_number() %>% log2 - 1,
              grepl(">",r_breakpoint) ~ r_breakpoint %>% as.character() %>% parse_number() %>% log2,
              TRUE ~ r_breakpoint %>% as.character() %>% parse_number() %>% log2 - 1
            )

            ecoff_line = case_when(
              grepl("≤", ecoff) ~ ecoff %>% as.character() %>% parse_number() %>% log2,
              grepl("=", ecoff) ~ ecoff %>% as.character() %>% parse_number() %>% log2,
              grepl("<", ecoff) ~ ecoff %>% as.character() %>% parse_number() %>% log2 - 1,
              TRUE ~ ecoff %>% as.character() %>% parse_number() %>% log2
            )

            if(s_line == ecoff_line){
              s_line = s_line + 0.05
            }

            mean = mean +
              ggnewscale::new_scale_color() +
              geom_hline(aes(yintercept = s_line, color = "Susceptible Breakpoint", linetype =  "Breakpoint")) +
              geom_hline(aes(yintercept = r_line, color = "Resistant Breakpoint", linetype =  "Breakpoint")) +
              geom_hline(aes(yintercept = ecoff_line, color = "ECOFF", linetype =  "Cutoff")) +

              scale_color_manual(breaks = c("Susceptible Breakpoint", "Resistant Breakpoint", "ECOFF"),
                                 labels = c(TeX(paste0("Susceptible Breakpoint: ", s_breakpoint,r'($\mu$)',"g/mL")), TeX(paste0("Resistant Breakpoint: ", r_breakpoint,r'($\mu$)',"g/mL")), TeX(paste0("ECOFF: ", ecoff,r'($\mu$)',"g/mL"))),
                                 values = c("#7CAE00", "#C77CFF", "#ffd700"), name = "Breakpoints and Cutoffs") +
              scale_linetype_manual(breaks=c("Fitted Model","Breakpoint", "Cutoff"), values=c(1,5,2), guide = "none")  #+ guides(linetype = "none", color = "none")

          }else if(
            is.na(ecoff) & !is.na(visual_split) &
            !(is.na(s_breakpoint) & is.na(r_breakpoint))
          ){
            pi_bounds = pi_bounds %>%
              mutate(.by = t,
                     c1vs = 1 - predict(lr_output_visual_split, newdata = tibble(t = t), type = "response"),
                     c2vs = predict(lr_output_visual_split, newdata = tibble(t = t), type = "response"),
                     susceptible = 1 - predict(lr_output_bkpt, newdata = tibble(t = t), type = "response"),
                     resistant = predict(lr_output_bkpt, newdata = tibble(t = t), type = "response") )


            if(!is.null(skip) && "bkpts" %in% skip){
              pi = pi +
                scale_color_manual(breaks = c("Component 1 Proportion", "Component 2 Proportion"), values = c("#e4190b", "#00999d"), labels = c(TeX(r'(Component 1 Prevalence: $\hat{\pi}_{1,t}$)'), TeX(r'(Component 2 Prevalence: $\hat{\pi}_{2,t}$)')), name = "Component Prevalence") +
                ggnewscale::new_scale_color() +
                geom_line(aes(x = offset_time_as_date(t, start_date), y = c1vs, color = "Below Split", linetype = "Logistic Regression"), data = pi_bounds) +
                geom_line(aes(x = offset_time_as_date(t, start_date), y = c2vs, color = "Above Split", linetype = "Logistic Regression"), data = pi_bounds) +
                #geom_function(fun = function(t){(1 - predict(lr_output_bkpt, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response"))}, aes(color = "Susceptible", linetype = "Logistic Regression")) +
                #geom_function(fun = function(t){predict(lr_output_bkpt, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response")}, aes(color = "Resistant", linetype = "Logistic Regression")) +
                #geom_function(fun = function(t){(1 - predict(lr_output_visual_split, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response"))}, aes(color = "Below Split", linetype = "Logistic Regression")) +
                #geom_function(fun = function(t){predict(lr_output_visual_split, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response")}, aes(color = "Above Split", linetype = "Logistic Regression")) +
                scale_color_manual(breaks = c(  "Below Split", "Above Split"), values = c( "#DF4601", "#000000"), name = "Other Prevalence") +  #+ guides(linetype = "none")
                scale_linetype_discrete(name = "Estimate Source")
            }else{

              pi = pi +
                scale_color_manual(breaks = c("Component 1 Proportion", "Component 2 Proportion"), values = c("#e4190b", "#00999d"), labels = c(TeX(r'(Component 1 Prevalence: $\hat{\pi}_{1,t}$)'), TeX(r'(Component 2 Prevalence: $\hat{\pi}_{2,t}$)')), name = "Component Prevalence") +
                ggnewscale::new_scale_color() +
                geom_line(aes(x = offset_time_as_date(t, start_date), y = susceptible, color = "Susceptible", linetype = "Logistic Regression"), data = pi_bounds) +
                geom_line(aes(x = offset_time_as_date(t, start_date), y = resistant, color = "Resistant", linetype = "Logistic Regression"), data = pi_bounds) +
                geom_line(aes(x = offset_time_as_date(t, start_date), y = c1vs, color = "Below Split", linetype = "Logistic Regression"), data = pi_bounds) +
                geom_line(aes(x = offset_time_as_date(t, start_date), y = c2vs, color = "Above Split", linetype = "Logistic Regression"), data = pi_bounds) +
                #geom_function(fun = function(t){(1 - predict(lr_output_bkpt, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response"))}, aes(color = "Susceptible", linetype = "Logistic Regression")) +
                #geom_function(fun = function(t){predict(lr_output_bkpt, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response")}, aes(color = "Resistant", linetype = "Logistic Regression")) +
                #geom_function(fun = function(t){(1 - predict(lr_output_visual_split, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response"))}, aes(color = "Below Split", linetype = "Logistic Regression")) +
                #geom_function(fun = function(t){predict(lr_output_visual_split, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response")}, aes(color = "Above Split", linetype = "Logistic Regression")) +
                scale_color_manual(breaks = c("Susceptible", "Resistant",  "Below Split", "Above Split"), values = c( "#7CAE00", "#C77CFF", "#DF4601", "#000000"), name = "Other Prevalence") +  #+ guides(linetype = "none")
                scale_linetype_discrete(name = "Estimate Source")
            }


            s_line = case_when(
              grepl("≤",s_breakpoint) ~ s_breakpoint %>% as.character() %>% parse_number() %>% log2,
              grepl("=",s_breakpoint) ~ s_breakpoint %>% as.character() %>% parse_number() %>% log2,
              grepl("<",s_breakpoint) ~ s_breakpoint %>% as.character() %>% parse_number() %>% log2 - 1,
              TRUE ~ s_breakpoint %>% as.character() %>% parse_number() %>% log2
            )
            r_line = case_when(
              grepl("≥",r_breakpoint) ~ r_breakpoint %>% as.character() %>% parse_number() %>% log2 - 1,
              grepl("=",r_breakpoint) ~ r_breakpoint %>% as.character() %>% parse_number() %>% log2 - 1,
              grepl(">",r_breakpoint) ~ r_breakpoint %>% as.character() %>% parse_number() %>% log2,
              TRUE ~ r_breakpoint %>% as.character() %>% parse_number() %>% log2 - 1
            )

            visual_split_line = case_when(
              grepl("≤", visual_split) ~ visual_split %>% as.character() %>% parse_number() %>% log2,
              grepl("=", visual_split) ~ visual_split %>% as.character() %>% parse_number() %>% log2,
              grepl("<", visual_split) ~ visual_split %>% as.character() %>% parse_number() %>% log2 - 1,
              TRUE ~ visual_split %>% as.character() %>% parse_number() %>% log2
            )

            if(s_line == visual_split_line){
              s_line = s_line + 0.05
            }

            mean = mean +
              ggnewscale::new_scale_color() +
              geom_hline(aes(yintercept = s_line, color = "Susceptible Breakpoint", linetype =  "Breakpoint")) +
              geom_hline(aes(yintercept = r_line, color = "Resistant Breakpoint", linetype =  "Breakpoint")) +
              geom_hline(aes(yintercept = visual_split_line, color = "Visual Split", linetype =  "Cutoff")) +

              scale_color_manual(breaks = c("Susceptible Breakpoint", "Resistant Breakpoint", "Visual Split"),
                                 labels = c(TeX(paste0("Susceptible Breakpoint: ", s_breakpoint,r'($\mu$)',"g/mL")), TeX(paste0("Resistant Breakpoint: ", r_breakpoint,r'($\mu$)',"g/mL")), TeX(paste0("Visual Split: ", visual_split,r'($\mu$)',"g/mL"))),
                                 values = c("#7CAE00", "#C77CFF", "#DF4601"), name = "Breakpoints and Cutoffs") +
              scale_linetype_manual(breaks=c("Fitted Model","Breakpoint", "Cutoff"), values=c(1,5,2), guide = "none")  #+ guides(linetype = "none", color = "none")

          }else{
            pi_bounds = pi_bounds %>%
              mutate(.by = t,
                     wt = 1 - predict(lr_output_ecoff, newdata = tibble(t = t), type = "response"),
                     nwt = predict(lr_output_ecoff, newdata = tibble(t = t), type = "response"),
                     c1vs = 1 - predict(lr_output_visual_split, newdata = tibble(t = t), type = "response"),
                     c2vs = predict(lr_output_visual_split, newdata = tibble(t = t), type = "response"),
                     susceptible = 1 - predict(lr_output_bkpt, newdata = tibble(t = t), type = "response"),
                     resistant = predict(lr_output_bkpt, newdata = tibble(t = t), type = "response") )


            if(!is.null(skip) && "bkpts" %in% skip & "ecoff" %in% skip){
              pi = pi +
                scale_color_manual(breaks = c("Component 1 Proportion", "Component 2 Proportion"), values = c("#e4190b", "#00999d"), labels = c(TeX(r'(Component 1 Prevalence: $\hat{\pi}_{1,t}$)'), TeX(r'(Component 2 Prevalence: $\hat{\pi}_{2,t}$)')), name = "Component Prevalence") +
                ggnewscale::new_scale_color() +
                geom_line(aes(x = offset_time_as_date(t, start_date), y = wt, color = "Below Split", linetype = "Logistic Regression"), data = pi_bounds) +
                geom_line(aes(x = offset_time_as_date(t, start_date), y = nwt, color = "Above Split", linetype = "Logistic Regression"), data = pi_bounds) +
                #geom_function(fun = function(t){(1 - predict(lr_output_bkpt, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response"))}, aes(color = "Susceptible", linetype = "Logistic Regression")) +
                #geom_function(fun = function(t){predict(lr_output_bkpt, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response")}, aes(color = "Resistant", linetype = "Logistic Regression")) +
                #geom_function(fun = function(t){(1 - predict(lr_output_ecoff, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response"))}, aes(color = "WT", linetype = "Logistic Regression")) +
                #geom_function(fun = function(t){predict(lr_output_ecoff, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response")}, aes(color = "NWT", linetype = "Logistic Regression")) +
                scale_color_manual(breaks = c("Below Split", "Above Split"), values = c( "#DF4601", "#000000"), name = "Other Prevalence") +  #+ guides(linetype = "none")
                scale_linetype_discrete(name = "Estimate Source")
            }else if(!is.null(skip) && "bkpts" %in% skip){
              pi = pi +
                scale_color_manual(breaks = c("Component 1 Proportion", "Component 2 Proportion"), values = c("#e4190b", "#00999d"), labels = c(TeX(r'(Component 1 Prevalence: $\hat{\pi}_{1,t}$)'), TeX(r'(Component 2 Prevalence: $\hat{\pi}_{2,t}$)')), name = "Component Prevalence") +
                ggnewscale::new_scale_color() +
                geom_line(aes(x = offset_time_as_date(t, start_date), y = wt, color = "WT", linetype = "Logistic Regression"), data = pi_bounds) +
                geom_line(aes(x = offset_time_as_date(t, start_date), y = nwt, color = "NWT", linetype = "Logistic Regression"), data = pi_bounds) +
                geom_line(aes(x = offset_time_as_date(t, start_date), y = wt, color = "Below Split", linetype = "Logistic Regression"), data = pi_bounds) +
                geom_line(aes(x = offset_time_as_date(t, start_date), y = nwt, color = "Above Split", linetype = "Logistic Regression"), data = pi_bounds) +
                #geom_function(fun = function(t){(1 - predict(lr_output_bkpt, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response"))}, aes(color = "Susceptible", linetype = "Logistic Regression")) +
                #geom_function(fun = function(t){predict(lr_output_bkpt, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response")}, aes(color = "Resistant", linetype = "Logistic Regression")) +
                #geom_function(fun = function(t){(1 - predict(lr_output_ecoff, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response"))}, aes(color = "WT", linetype = "Logistic Regression")) +
                #geom_function(fun = function(t){predict(lr_output_ecoff, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response")}, aes(color = "NWT", linetype = "Logistic Regression")) +
                scale_color_manual(breaks = c("WT", "NWT", "Below Split", "Above Split"), values = c(  "#fcbf07", "#0211a3", "#DF4601", "#000000"), name = "Other Prevalence") +  #+ guides(linetype = "none")
                scale_linetype_discrete(name = "Estimate Source")
            }else if(!is.null(skip) && "ecoff" %in% skip){
              pi = pi +
                scale_color_manual(breaks = c("Component 1 Proportion", "Component 2 Proportion"), values = c("#e4190b", "#00999d"), labels = c(TeX(r'(Component 1 Prevalence: $\hat{\pi}_{1,t}$)'), TeX(r'(Component 2 Prevalence: $\hat{\pi}_{2,t}$)')), name = "Component Prevalence") +
                ggnewscale::new_scale_color() +
                geom_line(aes(x = offset_time_as_date(t, start_date), y = susceptible, color = "Susceptible", linetype = "Logistic Regression"), data = pi_bounds) +
                geom_line(aes(x = offset_time_as_date(t, start_date), y = resistant, color = "Resistant", linetype = "Logistic Regression"), data = pi_bounds) +
                geom_line(aes(x = offset_time_as_date(t, start_date), y = wt, color = "Below Split", linetype = "Logistic Regression"), data = pi_bounds) +
                geom_line(aes(x = offset_time_as_date(t, start_date), y = nwt, color = "Above Split", linetype = "Logistic Regression"), data = pi_bounds) +
                #geom_function(fun = function(t){(1 - predict(lr_output_bkpt, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response"))}, aes(color = "Susceptible", linetype = "Logistic Regression")) +
                #geom_function(fun = function(t){predict(lr_output_bkpt, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response")}, aes(color = "Resistant", linetype = "Logistic Regression")) +
                #geom_function(fun = function(t){(1 - predict(lr_output_ecoff, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response"))}, aes(color = "WT", linetype = "Logistic Regression")) +
                #geom_function(fun = function(t){predict(lr_output_ecoff, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response")}, aes(color = "NWT", linetype = "Logistic Regression")) +
                scale_color_manual(breaks = c("Susceptible", "Resistant","Below Split", "Above Split"), values = c( "#7CAE00", "#C77CFF", "#DF4601", "#000000"), name = "Other Prevalence") +  #+ guides(linetype = "none")
                scale_linetype_discrete(name = "Estimate Source")
            }else{

              pi = pi +
                scale_color_manual(breaks = c("Component 1 Proportion", "Component 2 Proportion"), values = c("#e4190b", "#00999d"), labels = c(TeX(r'(Component 1 Prevalence: $\hat{\pi}_{1,t}$)'), TeX(r'(Component 2 Prevalence: $\hat{\pi}_{2,t}$)')), name = "Component Prevalence") +
                ggnewscale::new_scale_color() +
                geom_line(aes(x = offset_time_as_date(t, start_date), y = susceptible, color = "Susceptible", linetype = "Logistic Regression"), data = pi_bounds) +
                geom_line(aes(x = offset_time_as_date(t, start_date), y = resistant, color = "Resistant", linetype = "Logistic Regression"), data = pi_bounds) +
                geom_line(aes(x = offset_time_as_date(t, start_date), y = wt, color = "WT", linetype = "Logistic Regression"), data = pi_bounds) +
                geom_line(aes(x = offset_time_as_date(t, start_date), y = nwt, color = "NWT", linetype = "Logistic Regression"), data = pi_bounds) +
                geom_line(aes(x = offset_time_as_date(t, start_date), y = wt, color = "Below Split", linetype = "Logistic Regression"), data = pi_bounds) +
                geom_line(aes(x = offset_time_as_date(t, start_date), y = nwt, color = "Above Split", linetype = "Logistic Regression"), data = pi_bounds) +
                #geom_function(fun = function(t){(1 - predict(lr_output_bkpt, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response"))}, aes(color = "Susceptible", linetype = "Logistic Regression")) +
                #geom_function(fun = function(t){predict(lr_output_bkpt, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response")}, aes(color = "Resistant", linetype = "Logistic Regression")) +
                #geom_function(fun = function(t){(1 - predict(lr_output_ecoff, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response"))}, aes(color = "WT", linetype = "Logistic Regression")) +
                #geom_function(fun = function(t){predict(lr_output_ecoff, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response")}, aes(color = "NWT", linetype = "Logistic Regression")) +
                scale_color_manual(breaks = c("Susceptible", "Resistant",  "WT", "NWT", "Below Split", "Above Split"), values = c( "#7CAE00", "#C77CFF", "#fcbf07", "#0211a3", "#DF4601", "#000000"), name = "Other Prevalence") +  #+ guides(linetype = "none")
                scale_linetype_discrete(name = "Estimate Source")

            }


            s_line = case_when(
              grepl("≤",s_breakpoint) ~ s_breakpoint %>% as.character() %>% parse_number() %>% log2,
              grepl("=",s_breakpoint) ~ s_breakpoint %>% as.character() %>% parse_number() %>% log2,
              grepl("<",s_breakpoint) ~ s_breakpoint %>% as.character() %>% parse_number() %>% log2 - 1,
              TRUE ~ s_breakpoint %>% as.character() %>% parse_number() %>% log2
            )
            r_line = case_when(
              grepl("≥",r_breakpoint) ~ r_breakpoint %>% as.character() %>% parse_number() %>% log2 - 1,
              grepl("=",r_breakpoint) ~ r_breakpoint %>% as.character() %>% parse_number() %>% log2 - 1,
              grepl(">",r_breakpoint) ~ r_breakpoint %>% as.character() %>% parse_number() %>% log2,
              TRUE ~ r_breakpoint %>% as.character() %>% parse_number() %>% log2 - 1
            )

            ecoff_line = case_when(
              grepl("≤", ecoff) ~ ecoff %>% as.character() %>% parse_number() %>% log2,
              grepl("=", ecoff) ~ ecoff %>% as.character() %>% parse_number() %>% log2,
              grepl("<", ecoff) ~ ecoff %>% as.character() %>% parse_number() %>% log2 - 1,
              TRUE ~ ecoff %>% as.character() %>% parse_number() %>% log2
            )

            visual_split_line = case_when(
              grepl("≤", visual_split) ~ visual_split %>% as.character() %>% parse_number() %>% log2,
              grepl("=", visual_split) ~ visual_split %>% as.character() %>% parse_number() %>% log2,
              grepl("<", visual_split) ~ visual_split %>% as.character() %>% parse_number() %>% log2 - 1,
              TRUE ~ visual_split %>% as.character() %>% parse_number() %>% log2
            )

            if(visual_split_line == ecoff_line){
              visual_split_line = visual_split_line + 0.05
            }

            if(s_line == ecoff_line){
              s_line = s_line + 0.05
            }
            if(s_line == visual_split_line){
              s_line = s_line + 0.05
            }

            mean = mean +
              ggnewscale::new_scale_color() +
              geom_hline(aes(yintercept = s_line, color = "Susceptible Breakpoint", linetype =  "Breakpoint")) +
              geom_hline(aes(yintercept = r_line, color = "Resistant Breakpoint", linetype =  "Breakpoint")) +
              geom_hline(aes(yintercept = ecoff_line, color = "ECOFF", linetype =  "Cutoff")) +
              geom_hline(aes(yintercept = visual_split_line, color = "Visual Split", linetype =  "Cutoff")) +

              scale_color_manual(breaks = c("Susceptible Breakpoint", "Resistant Breakpoint", "ECOFF", "Visual Split"),
                                 labels = c(TeX(paste0("Susceptible Breakpoint: ", s_breakpoint,r'($\mu$)',"g/mL")), TeX(paste0("Resistant Breakpoint: ", r_breakpoint,r'($\mu$)',"g/mL")), TeX(paste0("ECOFF: ", ecoff,r'($\mu$)',"g/mL")), TeX(paste0("ECOFF: ", visual_split,r'($\mu$)',"g/mL"))),
                                 values = c("#7CAE00", "#C77CFF", "#ffd700", "#DF4601"), name = "Breakpoints and Cutoffs") +
              scale_linetype_manual(breaks=c("Fitted Model","Breakpoint", "Cutoff", "Cutoff"), values=c(1,5,2,2), guide = "none")  #+ guides(linetype = "none", color = "none")

          }

        }else{

        pi_bounds = tibble(t = seq(0, max(output$possible_data$t), len = 300),
                           pi2 = predict(output$pi_model, newdata = data.frame(t = t), type = "response"),
                           pi1 = 1 - pi2,
                           pi_se = predict(output$pi_model, newdata = data.frame(t = t), se.fit = TRUE)$se.fit,
                           pi_1_lp = logit(1 - predict(output$pi_model, newdata = data.frame(t = t), type = "response")),
                           pi_2_lp = predict(output$pi_model, newdata = data.frame(t = t)),
                           pi_1_lb = inverse_logit(pi_1_lp - (1.96 * pi_se)),
                           pi_1_ub = inverse_logit(pi_1_lp + (1.96 * pi_se)),
                           pi_2_lb = inverse_logit(pi_2_lp - (1.96 * pi_se)),
                           pi_2_ub = inverse_logit(pi_2_lp + (1.96 * pi_se))
        )

        mean = mean + guides(linetype = "none")
        pi = df %>%
          offset_time_as_date_in_df(., start_date) %>%
          ggplot(aes(x = t)) +
          geom_line(aes(x = offset_time_as_date(t, start_date), y = pi1, color = "Component 1 Proportion", linetype = "Fitted Model"), data = pi_bounds) +
          geom_line(aes(x = offset_time_as_date(t, start_date), y = pi2, color = "Component 2 Proportion", linetype = "Fitted Model"), data = pi_bounds) +
          #geom_function(fun = function(t){(1 - predict(output$pi_model, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response"))}, aes(color = "Component 1 Proportion", linetype = "Fitted Model")) +
          #geom_function(fun = function(t){predict(output$pi_model, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response")}, aes(color = "Component 2 Proportion", linetype = "Fitted Model")) +
          scale_color_manual(breaks = c("Component 1 Proportion", "Component 2 Proportion"), values = c("#e4190b", "#00999d"), labels = c(TeX(r'(Component 1 Prevalence: $\hat{\pi}_{1,t}$)'), TeX(r'(Component 2 Prevalence: $\hat{\pi}_{2,t}$)')), name = "Component Prevalence") +
          ylim(0,1)  +
          xlab("Time") + ylab("Proportion") + theme_minimal() + guides(linetype = "none")
        }


        pi = pi +
          geom_ribbon(aes(ymin = pi_1_lb, ymax = pi_1_ub, x = offset_time_as_date(t, start_date), fill = "Component 1 Proportion"), data = pi_bounds, alpha = 0.2) +
          geom_ribbon(aes(ymin = pi_2_lb, ymax = pi_2_ub, x = offset_time_as_date(t, start_date), fill = "Component 2 Proportion"), data = pi_bounds, alpha = 0.2) +
          scale_fill_manual(breaks = c("Component 1 Proportion", "Component 2 Proportion"), values = c("#e4190b", "#00999d"), labels = c(TeX(r'(Component 1 Prevalence: $\hat{\pi}_{1,t}$)'), TeX(r'(Component 2 Prevalence: $\hat{\pi}_{2,t}$)')), name = "Component Prevalence")

        if(!is.null(x_axis_t_breaks)){

          mean = mean + scale_x_continuous(breaks = x_axis_t_breaks %>% offset_time_as_date(., start_date = start_date), labels = x_axis_t_breaks %>% offset_time_as_date(., start_date = start_date) %>% year())
          pi = pi + scale_x_continuous(breaks = x_axis_t_breaks %>% offset_time_as_date(., start_date = start_date), labels = x_axis_t_breaks %>% offset_time_as_date(., start_date = start_date) %>% year())

        }

      return(patchwork::wrap_plots(mean,pi, ncol = 1))
    }
    else{
      warningCondition("No components converged")
      df %>% ggplot() +
        geom_segment(aes(x = t, xend = t, y = left_bound, yend = right_bound, color = cens), data = (df %>% filter(cens == "int")), alpha = 0.3) +
        geom_segment(aes(x = t, xend = t, y = right_bound, yend = left_bound, color = cens), data = (df %>% filter(cens == "lc") %>% mutate(left_bound = plot_min)), arrow = arrow(length = unit(0.03, "npc")), alpha = 0.3) +
        geom_segment(aes(x = t, xend = t, y = left_bound, yend = right_bound, color = cens), data = (df %>% filter(cens == "rc") %>% mutate(right_bound = plot_max)), arrow = arrow(length = unit(0.03, "npc")), alpha = 0.3) +
        geom_point(aes(x = t, y = left_bound,  color = cens), data = df %>% filter(left_bound != -Inf), alpha = 0.3) +
        geom_point(aes(x = t, y = right_bound,  color = cens), data = df %>% filter(right_bound != Inf), alpha = 0.3) +
        theme_minimal()

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

if(attr(df, "model") != "mgcv"){
    if(ncomp == 2){
      plot_min_2 <- min(sim_pi_survreg_boot(df, fit = output$mu_model[[1]], alpha = 0.05, nSims = 10000) %>% pull(lwr) %>% min(., na.rm = TRUE) - 0.2,
                        sim_pi_survreg_boot(df, fit = output$mu_model[[2]], alpha = 0.05, nSims = 10000) %>% pull(lwr) %>% min(., na.rm = TRUE) - 0.2)
    } else if(ncomp == 1){
      plot_min_2 <- sim_pi_survreg_boot(df, fit = fitted_comp, alpha = 0.05, nSims = 10000) %>% pull(lwr) %>% min(., na.rm = TRUE) - 0.2
    }else{
      plot_min_2 = plot_min_1
    }
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

    if(attr(df, "model") != "mgcv"){

    if(ncomp == 1){
      plot_max_2 <- sim_pi_survreg_boot(df, fit = fitted_comp, alpha = 0.05, nSims = 10000) %>% pull(upr) %>% max(., na.rm = TRUE) + 0.2
    } else if(ncomp == 2){
      plot_max_2 <- max(sim_pi_survreg_boot(df, fit = output$mu_model[[1]], alpha = 0.05, nSims = 10000) %>% pull(upr) %>% max(., na.rm = TRUE) + 0.2,
                        sim_pi_survreg_boot(df, fit = output$mu_model[[2]], alpha = 0.05, nSims = 10000) %>% pull(upr) %>% max(., na.rm = TRUE) + 0.2)
    }else{
      plot_max_2 = plot_max_1
    }

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
  if((length(models) == 1 && models == "Error")){
    return(TRUE)
  }else{is.na(models$scale) | (tibble(a = models$coefficients) %>% filter(is.na(a)) %>% nrow) > 0}
}

get_two_comp_ci = function(output){
  tibble(t = rep(seq(0, max(output$possible_data$t), len = 300), 2)) %>%
    mutate(
      c1pred = predict(output$mu_model[[1]], tibble(t), se = T)$fit,
      c1pred_se = predict(output$mu_model[[1]], tibble(t), se = T)$se.fit,
      c1pred_lb = c1pred - 1.96 * c1pred_se,
      c1pred_ub = c1pred + 1.96 * c1pred_se,
      c2pred = predict(output$mu_model[[2]], tibble(t), se = T)$fit,
      c2pred_se = predict(output$mu_model[[2]], tibble(t), se = T)$se.fit,
      c2pred_lb = c2pred - 1.96 * c2pred_se,
      c2pred_ub = c2pred + 1.96 * c2pred_se,
    ) %>% return()}

offset_time_as_date_in_df = function(df, start_date){
  df %>% mutate(t = offset_time_as_date(t, start_date)) %>% return()
}


inverse_logit = function(x){1 / (1 + (exp(-x)))}

logit = function(p){log(p / (1 - p))}
