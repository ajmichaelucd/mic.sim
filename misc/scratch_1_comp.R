fit_model_pi_1 = function(
    visible_data,

    formula = Surv(time = left_bound,
                   time2 = right_bound,
                   type = "interval2") ~ pspline(t, df = 0, calc = TRUE),
    formula2 = c == "2" ~ s(t), #or: c == "2" ~ lo(t)
    max_it = 3000,
    ncomp = 2,
    tol_ll = 1e-6,
    browse_at_end = FALSE,
    browse_each_step = FALSE,
    plot_visuals = FALSE,
    pi_link = "logit",
    #silent = FALSE,
    verbose = 3,
    #low_con = 2^-3,
    #high_con = 2^3,
    maxiter_survreg = 30,
    initial_weighting = 1 #smoothingspline or loess
){
  #verbose = 0: print nothing
  #verbose = 1: print run number (controlled outside in the purrr::map of this) --done
  #verbose = 2: print run number and iteration number --done
  #verbose = 3: print run number, iteration number, and iteration results --done
  #verbose = 4: print run number, iteration number, iteration results, and run aft as verbose
  #verbose = 0:


  #first E step-----

    possible_data <-
      visible_data %>%
      mutate(#visible data with c for component
        mid =
          case_when(
            left_bound == -Inf ~ right_bound - 0.5,
            right_bound == Inf ~ left_bound + 0.5,
            TRUE ~ (left_bound + right_bound) / 2
          ),
        rc = ifelse(right_bound == Inf, TRUE, FALSE)
      ) #%>%  ##this is probably only accurate for scale = "log"
    #print()

  newmodel  <- survival::survreg(
      formula,  ##Make this chunk into an argument of the function
      data = possible_data,
      dist = "gaussian",
      control = survreg.control(maxiter = maxiter_survreg, debug = verbose > 3))

  return(list(possible_data = possible_data,
              newmodel = newmodel))

}


drug = "CLINDA"

df = brd_mh %>%
  mutate(source = tolower(`Specimen Source`),
         source =
           case_when(
             grepl("lung", source) ~ "lower",
             grepl("lg", source) ~ "lower",
             grepl("nas", source) ~ "upper",
             grepl("phary", source) ~ "upper",
             grepl("lary", source) ~ "upper",
             grepl("trach", source) ~ "upper",
             grepl("traech", source) ~ "upper",
             grepl("dnps", source) ~ "upper",
             grepl("bronchus", source) ~ "lower",
             TRUE ~ source
           ),
         t = decimal_date(`Date of Isolation`) - 2007)




import_mics(df %>% pull(drug)) %>% mutate(left_bound = log2(left_bound),
                                          right_bound = log2(right_bound)) %>%
  mutate(
    cens =
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
      )) %>%
  tibble(., t = df$t) -> df_temp

low_con <- case_when(
  nrow(df_temp %>% filter(left_bound == -Inf)) == 0 ~ min(df_temp$left_bound) %>% as.numeric,
  TRUE ~ ifelse(length(df_temp %>% filter(left_bound == -Inf) %>% pull(right_bound) %>% unique) > 0, df_temp %>% filter(left_bound == -Inf) %>% pull(right_bound) %>% unique, NaN))

high_con <- case_when(
  nrow(df_temp %>% filter(right_bound == Inf)) == 0 ~ max(df_temp$right_bound),
  TRUE ~ ifelse(length(df_temp %>% filter(right_bound == Inf) %>% pull(left_bound) %>% unique) > 0, df_temp %>% filter(right_bound == Inf) %>% pull(left_bound) %>% unique, NaN))

df_temp %>% mutate(low_con = low_con, high_con = high_con) %>% mutate(obs_id = row_number()) %>% filter(!is.na(left_bound) & !is.na(right_bound)) -> visible_data

output <- fit_model_pi_1(visible_data = visible_data,
                       formula = Surv(time = left_bound,
                                      time2 = right_bound,
                                      type = "interval2") ~ pspline(t, df = 0, calc = TRUE),
                       formula2 = c == "2" ~ s(t),
                       max_it = 3000,
                       ncomp = 2,
                       tol_ll = 1e-06,
                       pi_link = "logit",
                       verbose = 3,
                       initial_weighting = 5)



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
  plot_min <- (df %>% filter(left_bound == -Inf) %>% pull(right_bound) %>% min) - 1
}else{
  plot_min <- (df %>% pull(left_bound) %>% min) - 1
}

if(nrow(df %>% filter(right_bound == Inf)) > 0){
  plot_max <- (df %>% filter(right_bound == Inf) %>% pull(left_bound) %>% max) + 1
}else{
  plot_max <- (df %>% pull(right_bound) %>% max) + 1
}

mu.se.brd.1 <- function(t, z){predict(output$newmodel, data.frame(t = t)) + z * predict(output$newmodel, data.frame(t = t), se = TRUE)$se.fit}

plot <- df %>% ggplot() +
  #ggnewscale::new_scale_color() +
  #geom_bar(aes(x = mid, fill = cens)) +
  geom_point(aes(x = t, y = mid, color = "Component 1 Mu"), data = df %>% filter(c == "2"), alpha = 0) +
  geom_segment(aes(x = t, xend = t, y = left_bound, yend = right_bound, color = "Component 1 Mu"), data = (df %>% filter(cens == "int")), alpha = 0.3)
if(nrow(df %>% filter(cens == "lc") %>% mutate(left_bound = right_bound - 1.5)) > 0){
plot <- plot + geom_segment(aes(x = t, xend = t, y = right_bound, yend = left_bound, color = "Component 1 Mu"), data = (df %>% filter(cens == "lc") %>% mutate(left_bound = right_bound - 1.5)), arrow = arrow(length = unit(0.03, "npc")), alpha = 0.3)}
if(nrow(df %>% filter(cens == "rc") %>% mutate(left_bound = right_bound - 1.5)) > 0){
  plot <- plot + geom_segment(aes(x = t, xend = t, y = left_bound, yend = right_bound, color = "Component 1 Mu"), data = (df %>% filter(cens == "rc") %>% mutate(right_bound = left_bound + 1.5)), arrow = arrow(length = unit(0.03, "npc")), alpha = 0.3)}
plot + geom_point(aes(x = t, y = left_bound), data = df %>% filter(left_bound != -Inf)) +
  geom_point(aes(x = t, y = right_bound), data = df %>% filter(right_bound != Inf)) +
  #scale_colour_gradientn(colours = c("purple", "orange")) +
  #ylim(plot_min - 0.5, plot_max + 0.5) +
  ggtitle(drug) +
  xlab("Time") +
  ylab("MIC") +
  geom_function(fun = function(t){predict(output$newmodel, newdata = data.frame(t = t))}, aes(color = "Component 1 Mu Model", linetype = "Fitted Model")) +
  geom_function(fun = function(t){mu.se.brd.1(t, z = 1.96)}, aes(color = "Component 1 Mu Model", linetype = "Fitted Model SE"), size = 0.6, alpha = 0.6) +
  geom_function(fun = function(t){mu.se.brd.1(t, z = -1.96)}, aes(color = "Component 1 Mu Model", linetype = "Fitted Model SE"), size = 0.6, alpha = 0.6)



output$newmodel$scale
output$newmodel[[2]]$scale


