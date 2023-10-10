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

prelim_cens_calc = function(visible_data){

  prelim_cens_check <- visible_data %>%
    mutate(
      cens = case_when(
        left_bound == -Inf | right_bound == low_con ~ "left_censored",
        right_bound == Inf |
          left_bound == high_con ~ "right_censored",
        TRUE ~ "interval_censored"
      )
    ) %>%
    summarise(.by = cens,
              n = n()) %>%
    mutate(
      n_tot = nrow(visible_data),
      proportion = n / n_tot,
      text_form = paste0("proportion ", cens, " is ", round(proportion, 4))
    )
  prelim_cens_check %>% pull(text_form) %>% cat(., sep = "\n")
  prelim_cens_check %>% filter(cens != "interval_censored") %>% pull(proportion) %>% sum %>% paste0("total sum of left-censored and right_censored observations is ", .) %>% print

}

prelim_cens_calc(visible_data)




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

fm_checks = function(single_model_output_fm){
  if (length(single_model_output_fm) == 1) {
    fm_convergence = FALSE
    print("did not converge")
  }else{
    fm_convergence = case_when(single_model_output_fm$converge %in% c("YES", "iterations") & ncomp > 1 ~ TRUE,
                               single_model_output_fm$ncomp == 1 ~ TRUE,
                               TRUE ~ FALSE)
    print("model converged, starting model checks")
    #model converged, so let's check the fit
  }


  if(fm_convergence & single_model_output_fm$ncomp > 1){
    max_scale <-
      max(single_model_output_fm$newmodel[[1]]$scale,
          single_model_output_fm$newmodel[[2]]$scale)
    max_difference_mu_hat <-
      max(abs(
        predict(single_model_output_fm$newmodel[[2]], newdata = visible_data) - predict(single_model_output_fm$newmodel[[1]], newdata = visible_data)
      ))
    if (max_scale ^ 2 > max_difference_mu_hat) {
      sigma_check = "stop"
      #censor_fm_check = NA_character_
      #fms_convergence = "tbd"
      if (verbose > 1) {print(paste0("sigma check results: ", sigma_check))}
    } else{

      sigma_check = "go"
      if (verbose > 1) {print(paste0("sigma check results: ", sigma_check))}
    }

    censor_fm_check <-
      check_for_excessive_censoring(single_model_output_fm, cutoff)

    if (verbose > 1) {print(paste0("censoring check results: ", censor_fm_check))}

    if (censor_fm_check == "BOTH") {
      fms_convergence = NA_character_
      single_model_output_fms = "PASS"
      if (verbose > 1) {
        print(
          "fit_model converged outside excessive censoring boundaries in both directions"
        )
      }
    } else if (censor_fm_check == "All Clear" & sigma_check == "go") {
      fms_convergence = NA_character_
      single_model_output_fms = "PASS"
      if (verbose > 1) {
        print("fit_model converged within excessive censoring boundaries")
      }
    } else if (censor_fm_check == "All Clear" & sigma_check == "stop") {
      fms_convergence = NA_character_
      single_model_output_fms = "PASS"
      if (verbose > 1) {
        print("fit_model converged within excessive censoring boundaries but violated sigma check, cannot find a side to use for fms")
      }
    } else{
      fms_convergence = "tbd"
    }
  }
  if(length(single_model_output_fm) > 1 & !fm_convergence & single_model_output_fm$ncomp > 1){ ##put the stuff for a model that didn't converge but generated output here, assign censor_fm_check as "RC", "LC", or both? skip sigma check

    sigma_check = NA_character_ #can't sigma check because one scale is missing
    censor_fm_check =
      case_when(
        is.na(single_model_output_fm$newmodel[[1]]$coefficients[1]) & is.na(single_model_output_fm$newmodel[[2]]$coefficients[1]) ~ "BOTH",
        is.na(single_model_output_fm$newmodel[[1]]$coefficients[1]) ~ "LC",
        is.na(single_model_output_fm$newmodel[[2]]$coefficients[1]) ~ "RC",
        TRUE ~ "BOTH"
      )
    fms_convergence = case_when(
      censor_fm_check == "BOTH" ~ NA,
      TRUE ~ "tbd"
    )
    if(is.na(fms_convergence)){single_model_output_fms = "PASS"}

  }
  if(length(single_model_output_fm) == 1 | single_model_output_fm$ncomp == 1){
    sigma_check = NA_character_
    censor_fm_check = NA_character_
    fms_convergence = "PASS"
  }
  return(
    list(
      sigma_check = sigma_check,
      censor_fm_check = censor_fm_check,
      fms_convergence = fms_convergence) )
}
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
