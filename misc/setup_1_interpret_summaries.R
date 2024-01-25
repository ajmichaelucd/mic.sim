library(gt)

batch_name = "setup_1"
setup_num = 1
date = "12212023"

low_con = -5
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
                          pi2_init = rep(c(0, 0.1, 0.2), 4),
                          sd_param = param_grid %>% pull(sd_vals) %>% unlist
)

data


data %>% summarize(.by = c(row, comp_conv),
                   n = n(),
                   pct = n()/10000) %>%
  left_join(.,( param_grid_added %>% select(row, sd1, sd2, pi2_init, sd_param))) %>%
  right_join(., expand.grid(comp_conv = c("both", "comp 1", "comp 2", "neither"),
                            row = 1:12
  ) %>% tibble) %>% select(-n) %>%
  pivot_wider(names_from = c(comp_conv), names_sep = "_", values_from = c(pct)) %>% filter(!is.na(sd1)) %>%
  replace(is.na(.), 0) %>% select(sd1, sd2, pi2_init, sd_param, both, `comp 1`, `comp 2`, neither, everything()) %>%
  gt(data = .) %>%
  cols_label(
    row = "Scenario",
    sd1 = "Component 1 SD",
    sd2 = "Component 2 SD",
    pi2_init = "Y-Intercept of Proportion Comp 2",
    sd_param = "Initial Parameter SD",
    both = "Both Components Converged",
    `comp 1` = "Only Component 1 Converged",
    `comp 2` = "Only Component 2 Converged",
    neither = "Neither Component Converged"
  ) %>%
  fmt_percent(
    columns = c(both, `comp 1`, `comp 2`, neither),
    decimals = 2,
    drop_trailing_zeros = TRUE
  ) %>% tab_header(
    title = "Simulation Set 1: Convergence Rates Under Different Population Proportions of Component 2 and Initial Standard Deviation Parameters",
    subtitle = glue::glue("With Maximum Tested Concentration of {high_con} on log2 Scale and n = 300")
  ) %>%
  tab_footnote(
    footnote = "Function for population proportion from component 2 is intercept + (0.002 * t) evaluated from t= 0 to t = 16",
    locations = cells_column_labels(
      columns = c(pi2_init)
    )
  ) %>%
  tab_footnote(
    footnote = "Initial parameter for standard deviation determines width of standard deviations for random starts to simulations, as well as variance of initial random mu starts",
    locations = cells_column_labels(
      columns = c(sd_param)
    )
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
) %>% right_join(( param_grid_added %>% select(row, sd1, sd2, pi2_init, sd_param)), .) %>%
  gt() %>%
  cols_label(
    row = "Scenario",
    sd1 = "Component 1 Sigma",
    sd2 = "Component 2 Sigma",
    pi2_init = "Y-Intercept of Proportion Comp 2",
    sd_param = "Initial Parameter SD",
    mean_step = "Mean Number of Steps to Converge",
    median_step = "Median Number of Steps to Converge",
    mean_like = "Mean Likelihood",
    median_like = "Median Likelihood",
    mean_c1_scale_est = "Mean Estimate of Comp 1 Sigma",
    mean_c2_scale_est = "Mean Estimate of Comp 2 Sigma",
    median_c1_scale_est = "Median Estimate of Comp 1 Scale",
    median_c2_scale_est = "Median Estimate of Comp 2 Scale",
    mean_abs_error_area_mu_1 = "Mean Total Error Area Comp 1 Mu",
    mean_abs_error_area_mu_2 = "Mean Total Error Area Comp 2 Mu",
    mean_abs_error_area_pi = "Mean Total Error Area Pi ",
    median_abs_error_area_mu_1 = "Median Total Error Area Comp 1 Mu",
    median_abs_error_area_mu_2 = "Median Total Error Area Comp 2 Mu",
    median_abs_error_area_pi = "Median Total Error Area Pi ",
    mean_bias_1 = "Comp 1 Mean Residuals",
    mean_bias_2 = "Comp 2 Mean Residuals",
  ) %>%
  cols_move_to_start(c(row, sd1, sd2, pi2_init, sd_param)) %>%
  cols_hide(c(median_like, mean_step, mean_c1_scale_est, mean_c2_scale_est)) %>%
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
    title = "Simulation Set 1: Error Metrics Under Different Population Proportions of Component 2 and Initial Standard Deviation Parameters",
    subtitle = glue::glue("With Maximum Tested Concentration of {high_con} on log2 Scale and n = 300")
  )






