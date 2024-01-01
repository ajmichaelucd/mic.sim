library(gt)

batch_name = "setup_3"
setup_num = 3
date = "12212023"

low_con = -3
high_con = 3

wd = "~/Desktop/Dissertation Project/Chapter 1/simulation_scripts"

#paste0(wd, "/", batch_name, "_", date, "_summary_", i, ".Rdata")

ld = function(i, wd, batch_name, date){
  loadRData(
    paste0(wd,
           "/",
           batch_name,
           "_",
           date,
           "_summary_",
           i,
           ".Rdata")
  ) %>%
    mutate(row = i) %>% return()
}

param_grid = loadRData("~/Desktop/Dissertation Project/Chapter 1/simulation_scripts/param_grid_list.Rdata")[[setup_num]]

data =
  map(1:12, ~ld(.x, wd, batch_name, date)) %>%
  data.table::rbindlist() %>%
  tibble
head1 = function(x){
  head(x, 1)
}
tail1 = function(x){
  tail(x, 1)
}

param_grid_added = tibble(param_grid %>% mutate(row = row_number()),
                          sd1 = (param_grid %>% pull(sd_vector) %>% lapply(tibble) %>% lapply(head1) %>% unlist %>% as_vector %>% unname),
                          sd2 = (param_grid %>% pull(sd_vector) %>% lapply(tibble) %>% lapply(tail1) %>% unlist %>% as_vector %>% unname),
                          mu1 = rep(c(rep(-1, 3), rep(0, 3)), 2),
                          mu2_min = rep(c(rep(1, 3), rep(2, 3)), 2),
                          pi2 = c(rep(0.3, 6), rep(0.5, 6))
)

data

param_grid$`E[X|T,C]`[[1]]
param_grid$`E[X|T,C]`[[6]]

param_grid$sd_vector[[1]] %>% unname
param_grid$sd_vector[[2]] %>% unname
param_grid$sd_vector[[3]] %>% unname

bounds = tibble(t = seq(0,16, by = 0.05)) %>%
  mutate(.by = t,
        row_1_c1_lb = param_grid$`E[X|T,C]`[[1]](t, 1) - 1.96 * (param_grid$sd_vector[[1]] %>% unname)[1],
         row_1_c1_ub = param_grid$`E[X|T,C]`[[1]](t, 1) + 1.96 * (param_grid$sd_vector[[1]] %>% unname)[1],
         row_1_c2_lb = param_grid$`E[X|T,C]`[[1]](t, 2) - 1.96 * (param_grid$sd_vector[[1]] %>% unname)[2],
         row_1_c2_ub = param_grid$`E[X|T,C]`[[1]](t, 2) + 1.96 * (param_grid$sd_vector[[1]] %>% unname)[2],
         row_2_c1_lb = param_grid$`E[X|T,C]`[[1]](t, 1) - 1.96 * (param_grid$sd_vector[[2]] %>% unname)[1],
         row_2_c1_ub = param_grid$`E[X|T,C]`[[1]](t, 1) + 1.96 * (param_grid$sd_vector[[2]] %>% unname)[1],
         row_2_c2_lb = param_grid$`E[X|T,C]`[[1]](t, 2) - 1.96 * (param_grid$sd_vector[[2]] %>% unname)[2],
         row_2_c2_ub = param_grid$`E[X|T,C]`[[1]](t, 2) + 1.96 * (param_grid$sd_vector[[2]] %>% unname)[2],
         row_3_c1_lb = param_grid$`E[X|T,C]`[[1]](t, 1) - 1.96 * (param_grid$sd_vector[[3]] %>% unname)[1],
         row_3_c1_ub = param_grid$`E[X|T,C]`[[1]](t, 1) + 1.96 * (param_grid$sd_vector[[3]] %>% unname)[1],
         row_3_c2_lb = param_grid$`E[X|T,C]`[[1]](t, 2) - 1.96 * (param_grid$sd_vector[[3]] %>% unname)[2],
         row_3_c2_ub = param_grid$`E[X|T,C]`[[1]](t, 2) + 1.96 * (param_grid$sd_vector[[3]] %>% unname)[2],
         row_4_c1_lb = param_grid$`E[X|T,C]`[[4]](t, 1) - 1.96 * (param_grid$sd_vector[[1]] %>% unname)[1],
         row_4_c1_ub = param_grid$`E[X|T,C]`[[4]](t, 1) + 1.96 * (param_grid$sd_vector[[1]] %>% unname)[1],
         row_4_c2_lb = param_grid$`E[X|T,C]`[[4]](t, 2) - 1.96 * (param_grid$sd_vector[[1]] %>% unname)[2],
         row_4_c2_ub = param_grid$`E[X|T,C]`[[4]](t, 2) + 1.96 * (param_grid$sd_vector[[1]] %>% unname)[2],
         row_5_c1_lb = param_grid$`E[X|T,C]`[[4]](t, 1) - 1.96 * (param_grid$sd_vector[[2]] %>% unname)[1],
         row_5_c1_ub = param_grid$`E[X|T,C]`[[4]](t, 1) + 1.96 * (param_grid$sd_vector[[2]] %>% unname)[1],
         row_5_c2_lb = param_grid$`E[X|T,C]`[[4]](t, 2) - 1.96 * (param_grid$sd_vector[[2]] %>% unname)[2],
         row_5_c2_ub = param_grid$`E[X|T,C]`[[4]](t, 2) + 1.96 * (param_grid$sd_vector[[2]] %>% unname)[2],
         row_6_c1_lb = param_grid$`E[X|T,C]`[[4]](t, 1) - 1.96 * (param_grid$sd_vector[[3]] %>% unname)[1],
         row_6_c1_ub = param_grid$`E[X|T,C]`[[4]](t, 1) + 1.96 * (param_grid$sd_vector[[3]] %>% unname)[1],
         row_6_c2_lb = param_grid$`E[X|T,C]`[[4]](t, 2) - 1.96 * (param_grid$sd_vector[[3]] %>% unname)[2],
         row_6_c2_ub = param_grid$`E[X|T,C]`[[4]](t, 2) + 1.96 * (param_grid$sd_vector[[3]] %>% unname)[2]
         )

b = bounds %>%
  ggplot() +
  ylim(-5, 8) +
  geom_function(fun = function(t){param_grid$`E[X|T,C]`[[4]](t, 1)}, aes(color = "C1 Mu = 0")) +
  geom_function(fun = function(t){param_grid$`E[X|T,C]`[[4]](t, 2)}, aes(color = "C2 Mu min = 2")) +
  geom_ribbon(aes(x = t, ymin = row_4_c1_lb, ymax = row_4_c1_ub, fill = "C1 SD = 1"), alpha = 0.3) +
  geom_ribbon(aes(x = t, ymin = row_6_c2_lb, ymax = row_6_c2_ub, fill = "C2 SD = 1.25"), alpha = 0.3) +
  geom_ribbon(aes(x = t, ymin = row_5_c2_lb, ymax = row_5_c2_ub, fill = "C2 SD = 1"), alpha = 0.4) +
  geom_ribbon(aes(x = t, ymin = row_4_c2_lb, ymax = row_4_c2_ub, fill = "C2 SD = 0.75"), alpha = 0.5) +
  geom_hline( aes(yintercept = high_con, color = "Limits of Tested Range"), linetype = "dashed") +
  geom_hline(aes(yintercept = low_con, color = "Limits of Tested Range"), linetype = "dashed") +
  scale_fill_manual(c(
    "C2 SD = 0.75",
    "C2 SD = 1",
    "C2 SD = 1.25",
    "C1 SD = 1"
  ),
  values = c(
    "pink",
    "blue",
    "darkgreen",
    "orange"
  )) +
  scale_color_manual(c("C1 Mu = 0", "C2 Mu min = 2", "Limits of Tested Range"),
                     values = c("red", "purple", "black")) +
  guides(color = guide_legend(title="Mu"),
         fill = guide_legend(title = "Sigma")) +
  ggtitle("Scenarios 4, 5, 6, 10, 11, 12")


a = bounds %>%
  ggplot() +
  ylim(-5, 8) +
  geom_function(fun = function(t){param_grid$`E[X|T,C]`[[1]](t, 1)}, aes(color = "C1 Mu = -1")) +
  geom_function(fun = function(t){param_grid$`E[X|T,C]`[[1]](t, 2)}, aes(color = "C2 Mu min = 1")) +
  geom_ribbon(aes(x = t, ymin = row_1_c1_lb, ymax = row_1_c1_ub, fill = "C1 SD = 1"), alpha = 0.3) +
  geom_ribbon(aes(x = t, ymin = row_3_c2_lb, ymax = row_3_c2_ub, fill = "C2 SD = 1.25"), alpha = 0.3) +
  geom_ribbon(aes(x = t, ymin = row_2_c2_lb, ymax = row_2_c2_ub, fill = "C2 SD = 1"), alpha = 0.4) +
  geom_ribbon(aes(x = t, ymin = row_1_c2_lb, ymax = row_1_c2_ub, fill = "C2 SD = 0.75"), alpha = 0.5) +
  geom_hline( aes(yintercept = high_con, color = "Limits of Tested Range"), linetype = "dashed") +
  geom_hline(aes(yintercept = low_con, color = "Limits of Tested Range"), linetype = "dashed") +
  scale_fill_manual(c(
    "C2 SD = 0.75",
    "C2 SD = 1",
    "C2 SD = 1.25",
    "C1 SD = 1"
  ),
  values = c(
    "pink",
    "blue",
    "darkgreen",
    "orange"
  )) +
  scale_color_manual(c("C1 Mu = -1", "C2 Mu min = 1", "Limits of Tested Range"),
                    values = c("red", "purple", "black"))  +
  guides(color = guide_legend(title="Mu"),
         fill = guide_legend(title = "Sigma"))  +
  ggtitle("Scenarios 1, 2, 3, 7, 8, 9")
#1: mu -1 and U, sd 1 and .75, pi = 0.3
#2: mu -1 and U, sd 1 and 1, pi = 0.3
#3: mu -1 and U, sd 1 and 1.25, pi = 0.3
#4: mu 0 and U, sd 1 and .75, pi = 0.3
#5: mu 0 and U, sd 1 and 1, pi = 0.3
#6: mu 0 and U, sd 1 and 1.25, pi = 0.3
#7: mu -1 and U, sd 1 and .75, pi = 0.5
#8: mu -1 and U, sd 1 and 1, pi = 0.5
#9: mu -1 and U, sd 1 and 1.25, pi = 0.5
#10: mu 0 and U, sd 1 and .75, pi = 0.5
#11: mu 0 and U, sd 1 and 1, pi = 0.5
#12: mu 0 and U, sd 1 and 1.25, pi = 0.5

a/b

conv_table = data %>% summarize(.by = c(row, comp_conv),
                   n = n(),
                   pct = n()/10000) %>%
  right_join(., expand.grid(comp_conv = c("both", "comp 1", "comp 2", "neither"),
                            row = 1:12
  ) %>% tibble) %>% select(-n) %>%
  pivot_wider(names_from = c(comp_conv), names_sep = "_", values_from = c(pct)) %>%
  replace(is.na(.), 0) %>%
  select(row, both, neither) %>% rename(
    pct_conv = both
  )

data %>% summarize(.by = row,
                   mean_step = mean(step, na.rm = TRUE),
                   median_step = median(step, na.rm = TRUE),
                   mean_like = mean(likelihood, na.rm = TRUE),
                   median_like = median(likelihood, na.rm = TRUE),
                   mean_c1_scale_est = mean(c1_scale_est, na.rm = TRUE),
                   mean_c2_scale_est = mean(c2_scale_est, na.rm = TRUE),
                   median_c1_scale_est = median(c1_scale_est, na.rm = TRUE),
                   median_c2_scale_est = median(c2_scale_est, na.rm = TRUE),
                   mean_abs_error_area_mu_1 = mean(abs_error_area_mu_1, na.rm = TRUE),
                   mean_abs_error_area_mu_2 = mean(abs_error_area_mu_2, na.rm = TRUE),
                   mean_abs_error_area_pi = mean(abs_error_area_pi, na.rm = TRUE),
                   median_abs_error_area_mu_1 = median(abs_error_area_mu_1, na.rm = TRUE),
                   median_abs_error_area_mu_2 = median(abs_error_area_mu_2, na.rm = TRUE),
                   median_abs_error_area_pi = median(abs_error_area_pi, na.rm = TRUE),
                   mean_bias_1 = mean(bias_1, na.rm = TRUE),
                   mean_bias_2 = mean(bias_2, na.rm = TRUE),
                   mean_abs_error_area_pi = mean(abs_error_area_pi, na.rm = TRUE)
) %>% right_join(( param_grid_added %>% select(row, sd1, sd2, mu1, mu2_min, pi2)), .) %>%
  left_join(., conv_table, by = "row") %>%
  gt() %>%
  cols_label(
    row = "Scenario",
    sd1 = "Comp 1 SD",
    sd2 = "Comp 2 SD",
    mu1 = "Comp 1 Mu",
    mu2_min = "Comp 2 Mu Minimum",
    #pct_censored = "Proportion of Component 2 that is Right Censored",
    pi2 = "Population Proportion Comp 2",
    pct_conv = "Runs Converged",
    mean_step = "Mean Number of Steps to Converge",
    median_step = "Median Number of Steps to Converge",
    mean_like = "Mean Likelihood",
    median_like = "Median Likelihood",
    mean_c1_scale_est = "Mean Estimate of Comp 1 Scale",
    mean_c2_scale_est = "Mean Estimate of Comp 2 Scale",
    median_c1_scale_est = "Median Estimate of Comp 1 Scale",
    median_c2_scale_est = "Median Estimate of Comp 2 Scale",
    mean_abs_error_area_mu_1 = "Mean Total Error Area Comp 1 Mu",
    mean_abs_error_area_mu_2 = "Mean Total Error Area Comp 2 Mu",
    mean_abs_error_area_pi = "Mean Total Error Area Pi ",
    median_abs_error_area_mu_1 = "Median Total Error Area Comp 1 Mu",
    median_abs_error_area_mu_2 = "Median Total Error Area Comp 2 Mu",
    median_abs_error_area_pi = "Mean Total Error Area Pi ",
    mean_bias_1 = "Comp 1 Mean Residuals",
    mean_bias_2 = "Comp 2 Mean Residuals",
  ) %>%
  cols_move_to_start(c(row, sd1, sd2, mu1, mu2_min, pi2, pct_conv)) %>%
  cols_hide(c(median_like, mean_step, median_c1_scale_est, median_c2_scale_est, neither)) %>%
  tab_footnote(
    footnote = "Residuals for Components took the difference between the estimated mu and the uncensored underlying value for all observations drawn from component 2 during simulation",
    locations = cells_column_labels(
      columns = c(mean_bias_1, mean_bias_2)
    )
  ) %>%
  tab_footnote(
    footnote = "Total area is calculated by taking the integral of the absolute value of the difference of the estimated curve for either mu or pi and the corresponding true curve from t= 0 to t = 16",
    locations = cells_column_labels(
      columns = c(mean_abs_error_area_mu_1, mean_abs_error_area_mu_2, mean_abs_error_area_pi, median_abs_error_area_mu_1, median_abs_error_area_mu_2, median_abs_error_area_pi)
    )
  )  %>% tab_header(
    title = "Scenario 3: Error Metrics For Varying Values of Component 2 Standard Deviation And Mu",
    subtitle = glue::glue("With Maximum Tested Concentration of {high_con} on log2 Scale and n = 300, and a Parabolic Component 2 of the form y = 0.05 * (t - 8)^2 + Mu Minimum")
  ) %>%
  fmt_percent(columns = c(pct_conv),
              decimals = 2,
              drop_trailing_zeros = TRUE)






