library(gt)

batch_name = "setup_2"
date = "12212023"

high_con = 3
low_con = -5

wd = "~/Desktop/Dissertation Project/Chapter 1/simulation_scripts"

paste0(wd, "/", batch_name, "_", date, "_summary_", i, ".Rdata")

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

param_grid = loadRData("~/Desktop/Dissertation Project/Chapter 1/simulation_scripts/param_grid_list.Rdata")[[2]]

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
      mu2 = c(rep(2.5, 2), rep(3, 2), rep(3.25, 2), rep(3.5, 2), rep(3.75, 2), rep(4, 2))
)

data %>% summarize(.by = row,
                   mean_like = mean(likelihood, na.rm = TRUE),
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
                   mean_bias_2 = mean(bias_2, na.rm = TRUE)
                   ) %>% right_join(( param_grid_added %>% select(row, sd1, sd2, mu2)), .) %>%
  mutate(
    pct_censored = (pnorm(high_con, mu2, sd2, lower.tail = FALSE))
  ) %>% select(row, sd1, sd2, mu2, pct_censored, everything()) %>%
  gt(data = .) %>%
  cols_label(
    row = "Scenario",
    sd2 = "Comp 2 Sigma",
    mu2 = "Comp 2 Mu",
    pct_censored = "Proportion of Comp 2 that is RC",
    mean_like = "Mean Likelihood",
    mean_c1_scale_est = "Mean Estimate of Comp 1 Sigma (SD)",
    mean_c2_scale_est = "Mean Estimate of Comp 2 Sigma (SD)",
    median_c2_scale_est = "Median Estimate of Comp 2 Scale",
    mean_abs_error_area_mu_1 = "Mean Total Error Area Comp 1 Mu",
    mean_abs_error_area_mu_2 = "Mean Total Error Area Comp 2 Mu",
    mean_abs_error_area_pi = "Mean Total Error Area Pi ",
    median_abs_error_area_mu_1 = "Median Total Error Area Comp 1 Mu",
    median_abs_error_area_mu_2 = "Median Total Error Area Comp 2 Mu",
    median_abs_error_area_pi = "Median Total Error Area Pi ",
    mean_bias_2 = "Comp 2 Mean Residuals"

  ) %>%
  cols_hide(c(median_c1_scale_est, sd1)) %>%
  tab_footnote(
    footnote = "RC: Right-Censored",
    locations = cells_column_labels(
      columns = c(mean_bias_2)
    )
  )  %>%
  tab_footnote(
    footnote = "Total area is calculated by taking the integral of the absolute value of the difference of the estimated curve for either mu or pi and the corresponding true curve from t= 0 to t = 16",
    locations = cells_column_labels(
      columns = c(pct_censored)
    )
  ) %>%
  tab_footnote(
    footnote = "Total area is calculated by taking the integral of the absolute value of the difference of the estimated curve for either mu or pi and the corresponding true curve from t= 0 to t = 16",
    locations = cells_column_labels(
      columns = c(mean_abs_error_area_mu_1, mean_abs_error_area_mu_2, mean_abs_error_area_pi, median_abs_error_area_mu_1, median_abs_error_area_mu_2, median_abs_error_area_pi)
    )
  )  %>% tab_header(
    title = "Simulation Set 2: Component 2 Error Metrics Under Different Levels of Censoring",
    subtitle = glue::glue("With Maximum Tested Concentration of {high_con} on log2 Scale, n = 300, a Fully Interval Censored Component 1, and the Population Proportion of Observations From Component 2 = 0.3")
  ) %>%
  fmt_percent(
    columns = c(pct_censored),
    decimals = 2,
    drop_trailing_zeros = TRUE
  )




  data %>% summarize(.by = c(row, comp_conv),
                     n = n(),
                     pct = n()/10000) %>%
    left_join(.,( param_grid_added %>% select(row, sd1, sd2, mu2))) %>%
    right_join(., expand.grid(comp_conv = c("both", "comp 1", "comp 2", "neither"),
                              row = 1:12
                              ) %>% tibble) %>% select(-n) %>%
    pivot_wider(names_from = c(comp_conv), names_sep = "_", values_from = c(pct)) %>% filter(!is.na(sd1)) %>% select(-row) %>%
   replace(is.na(.), 0) %>% mutate(
     pct_censored = (pnorm(high_con, mu2, sd2, lower.tail = FALSE))
   ) %>% select(sd1, sd2, mu2, pct_censored, everything()) %>%
  gt(data = .) %>%
  cols_label(
    sd1 = "Component 1 SD",
    sd2 = "Component 2 SD",
    mu2 = "Component 2 Mu",
    pct_censored = "Proportion of Component 2 that is Right Censored",
    both = "Both Components Converged",
    `comp 1` = "Only Component 1 Converged",
    `comp 2` = "Only Component 2 Converged",
    neither = "Neither Component Converged"
  ) %>%
  fmt_percent(
    columns = c(both, `comp 1`, `comp 2`, neither, pct_censored),
    decimals = 2,
    drop_trailing_zeros = TRUE
  ) %>% tab_header(
    title = "Simulation Set 2: Convergence Rates Under Different Censoring Levels for Component 2",
    subtitle = glue::glue("With Maximum Tested Concentration of {high_con} on log2 Scale, n = 300, a Fully Interval Censored Component 1, and the Population Proportion of Observations From Component 2 = 0.3")
  )

