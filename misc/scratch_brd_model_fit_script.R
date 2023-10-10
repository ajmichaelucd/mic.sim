#data
#name of mic column(s)

data = brd_mh
mic_col = "TULATH"
id_col = "Unique ID"
date_col = "Date of Isolation"
date_type = "decimal" #or "year"
covariate_vector = c("source")
start_time = 2007 #in decimal years (or just year if date_type == "year

drug = "TULATH"
bug = "mh"
primary_model_parameters = list(formula = Surv(time = left_bound,
                                               time2 = right_bound,
                                               type = "interval2") ~ pspline(t, df = 0, calc = TRUE),
                                formula2 = c == "2" ~ s(t),
                                max_it = 300,
                                ncomp = 2,
                                tol_ll = 1e-06,
                                pi_link = "logit",
                                verbose = 3,
                                initial_weighting = 8,
                                browse_each_step = FALSE,
                                plot_visuals = FALSE)


if(length(mic_col) == 1){
  mics = data %>% pull(mic_col) %>%
    import_mics() %>%
    mutate(left_bound = log2(left_bound), right_bound = log2(right_bound))
} else if(length(mic_col) == 2){
  a = data %>% pull(mic_col[1])
  b = data %>% pull(mic_col[2])
    mics = import_mics(mic_column = a, code_column = b) %>%
      mutate(left_bound = log2(left_bound), right_bound = log2(right_bound))
} else{
  errorCondition("mic_col should be a character vector of length one (if no separate column for code) or two (mic, code)")
}

  if(date_type == "decimal"){
    covariates = data %>% rename(date = date_col) %>%
      mutate(t = decimal_date(date) - start_time) %>%
      select(id_col, t, covariate_vector) %>%
      rename(id = id_col)
  } else if(date_type == "year"){
    covariates = data %>% rename(date = date_col) %>%
      mutate(t = as.numeric(date) - start_time) %>%
      select(id_col, t, covariate_vector) %>%
      rename(id = id_col)
    }else{
    errorCondition("pick decimal or year")
  }

df_temp = tibble(covariates, mics) %>%
  filter(!is.na(left_bound))


low_con <- case_when(
  nrow(df_temp %>% filter(left_bound == -Inf)) == 0 ~ min(df_temp$left_bound),
  TRUE ~ ifelse(nrow(df_temp %>% filter(left_bound == -Inf)) == 0, 0, df_temp %>% filter(left_bound == -Inf) %>% pull(right_bound) %>% unique))

high_con <- case_when(
  nrow(df_temp %>% filter(right_bound == Inf)) == 0 ~ max(df_temp$right_bound),
  TRUE ~ ifelse( nrow(df_temp %>% filter(right_bound == Inf)) == 0, 0, df_temp %>% filter(right_bound == Inf) %>% pull(left_bound) %>% unique))

visible_data = df_temp %>%
  mutate(low_con = low_con, high_con = high_con) %>%
  mutate(obs_id = row_number()) %>%
  filter(!is.na(left_bound) & !is.na(right_bound))


find_censoring_levels(visible_data)




single_model_output_fm_2 <- visible_data %>%
  fit_model_pi(visible_data = .,
               formula = primary_model_parameters$formula,
               formula2 = primary_model_parameters$formula2,
               max_it = primary_model_parameters$max_it,
               ncomp = 2,
               tol_ll = primary_model_parameters$tol_ll,
               pi_link = primary_model_parameters$pi_link,
               verbose = primary_model_parameters$verbose,
               initial_weighting = primary_model_parameters$initial_weighting,
               browse_each_step = primary_model_parameters$browse_each_step,
               plot_visuals = primary_model_parameters$plot_visuals
  )

single_model_output_fm_1 <- visible_data %>%
  fit_model_pi(visible_data = .,
               formula = primary_model_parameters$formula,
               formula2 = primary_model_parameters$formula2,
               max_it = primary_model_parameters$max_it,
               ncomp = 1,
               tol_ll = primary_model_parameters$tol_ll,
               pi_link = primary_model_parameters$pi_link,
               verbose = primary_model_parameters$verbose,
               initial_weighting = primary_model_parameters$initial_weighting,
               browse_each_step = primary_model_parameters$browse_each_step,
               plot_visuals = primary_model_parameters$plot_visuals
  )
single_model_output_fm_2
single_model_output_fm_1


fm_checks(single_model_output_fm_2)
fm_checks(single_model_output_fm_1)

#plot_fm here for each?

#then request user feeback

plot_fm(single_model_output_fm_2, paste0(drug, "-", stringr::str_to_upper(bug), " FM2"))
plot_fm(single_model_output_fm_1, paste0(drug, "-", stringr::str_to_upper(bug), " FM1"))





print(fm_checks(single_model_output_fm_2)$censor_fm_check)

fms_run = TRUE
cens_dir = "RC"



single_model_output_fms_2 <- fit_model_safety_pi(visible_data = visible_data,
                                  formula = primary_model_parameters$formula,
                                  formula2 = primary_model_parameters$formula2,
                                  max_it = primary_model_parameters$max_it,
                                  fm_check = cens_dir,
                                  ncomp = 2,
                                  tol_ll = primary_model_parameters$tol_ll,
                                  pi_link = primary_model_parameters$pi_link,
                                  verbose = primary_model_parameters$verbose,
                                  browse_each_step = primary_model_parameters$browse_each_step,
                                  plot_visuals = primary_model_parameters$plot_visuals
)

plot_fms(single_model_output_fms_2, title = paste0(drug, "-", stringr::str_to_upper(bug), " FMS2"))





plot_likelihood(single_model_output_fm_2$likelihood, format = "tibble")
