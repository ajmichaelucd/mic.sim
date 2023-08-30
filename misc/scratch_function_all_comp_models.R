visible_data =
  simulate_mics(
    #changed to test
    n = 300,
    t_dist = function(n){runif(n, min = 0, max = 16)},
    pi = function(t) {
      m1 <- -1.5 + 0.1 * t
      z1 <- (1+ exp(-m1))^-1 #if exp(m) gets large, it won't add the 1 so we write like this
      m2 <- -1 - 0.008 * t
      z2 <- (1+ exp(-m2))^-1
      tibble("1" = 1 - z1 - z2, "2" = z1, "3" = z2)
    },
    `E[X|T,C]` = function(t, c)
    {
      case_when(
        c == "1" ~ -4 + 0.01 * t,
        c == "2" ~ -1 + 0.02 * t,
        c == "3" ~ 2 - 0.02 * t,
        TRUE ~ NaN
      )
    },
    sd_vector = c("1" = 0.5, "2" = 0.5, "3" = 0.5),
    covariate_list = NULL,
    covariate_effect_vector =  c(0),
    conc_limits_table = NULL,
    low_con = -4,
    high_con = 2,
    scale = "log"
  ) %>% prep_sim_data_for_em(
    .,
    left_bound_name = "left_bound",
    right_bound_name = "right_bound",
    time = "t",
    covariate_names = NULL,
    scale = "log",
    keep_truth = keep_true_values,
    observed_value_name = "observed_value",
    comp_name = "comp"
  )
preview_visible_data = function(visible_data){
visible_data %>%
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
  filter(!is.na(left_bound)) -> df

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


  #   df %>% ggplot() +
  #   #geom_bar(aes(x = mid, fill = cens)) +
  #   geom_segment(aes(x = t, xend = t, y = left_bound, yend = right_bound, color = cens)) +
  #     geom_point(aes(x = t, y = left_bound), data = df %>% filter(left_bound != -Inf)) +
  #     geom_point(aes(x = t, y = right_bound), data = df %>% filter(right_bound != Inf)) +
  #   ylim(plot_min, plot_max) +
  #   ggtitle(column) +
  #     xlab("Time") +
  #     ylab("MIC")


  df %>% ggplot() +
    #geom_bar(aes(x = mid, fill = cens)) +
    geom_point(aes(x = t, y = mid, color = cens), data = df, alpha = 0) +
    geom_segment(aes(x = t, xend = t, y = left_bound, yend = right_bound, color = cens), data = (df %>% filter(left_bound != -Inf & right_bound != Inf))) +
    geom_segment(aes(x = t, xend = t, y = right_bound, yend = left_bound, color = cens), data = (df %>% filter(left_bound == -Inf & right_bound != Inf) %>% mutate(left_bound = right_bound - 1.5)), arrow = arrow(length = unit(0.03, "npc"))) +
    geom_segment(aes(x = t, xend = t, y = left_bound, yend = right_bound, color = cens), data = (df %>% filter(left_bound != -Inf & right_bound == Inf) %>% mutate(right_bound = left_bound + 1.5)), arrow = arrow(length = unit(0.03, "npc"))) +
    geom_point(aes(x = t, y = left_bound), data = df %>% filter(left_bound != -Inf)) +
    geom_point(aes(x = t, y = right_bound), data = df %>% filter(right_bound != Inf)) +
    ylim(plot_min - 0.5, plot_max + 0.5) +
    xlab("Time") +
    ylab("MIC")

}
preview_visible_data(visible_data)


formula = Surv(time = left_bound,
               time2 = right_bound,
               type = "interval2") ~ pspline(t, df = 0, calc = TRUE)
formula2 = c == "2" ~ s(t)
formula3 = list(cc ~ s(t), ~ s(t))
max_it = 3000
ncomp = 2
tol_ll = 1e-6
pi_link = "logit"
verbose = 3
maxiter_survreg = 30



output_1 = fit_model_pi(visible_data = visible_data,
              formula = formula,
              formula2 = formula2,
              max_it = max_it,
             ncomp = 1,
              tol_ll = tol_ll,
              pi_link = "logit",
              verbose = 3,
              maxiter_survreg = maxiter_survreg,
              initial_weighting = 1)

output_2 = fit_model_pi(visible_data = visible_data,
             formula = formula,
             formula2 = formula2,
             max_it = max_it,
             ncomp = 2,
             tol_ll = tol_ll,
             pi_link = "logit",
             verbose = 3,
             maxiter_survreg = maxiter_survreg,
             initial_weighting = 8)

##add checks for 2

##fix 3 then add 3 plus checks and safety mode
