#' Title
#'
#' @param results
#' @param settings
#'
#' @return
#' @export
#'
#' @importFrom tidyr complete
#' @importFrom magrittr %>%
#' @importFrom dplyr case_when mutate reframe filter summarise summarize group_by select rename ungroup
#' @importFrom tidyselect everything
#' @importFrom tibble tibble as_tibble
#' @importFrom stats predict
#'
#' @examples
capture_error_measures_one_run <- function(results, settings){
  #first: Error? If so note this and pass this to end
  #If not, are we looking at the findings of fit_model_pi or fit_model_safety_pi
  #function for fit_model_pi results
  ###first identify wt vs nonwt
  #function for fit_model_safety_pi results

#print(results$i)

  #work on fit_model_pi results-----------------



print(results$i)
  ##check directionality-----------


  results$run_status_notes %>% as.list -> run_status_notes

  #which results are we working with here-----------------
  situation <- case_when(
    length(results$single_model_output$single_model_output_fms) > 1 ~ "fms",
    length(results$single_model_output$single_model_output_fms) == 1 &&
      (!is.na(run_status_notes$fms_convergence) &&
         run_status_notes$fms_convergence == "FALSE") ~ "fm, fms failed",
    #length(results$single_model_output$single_model_output_fm) > 1 &&
    #  (is.na(run_status_notes$fms_convergence)
    run_status_notes$fm_convergence == "TRUE" & run_status_notes$sigma_check == "go" & run_status_notes$censor_fm_check == "All Clear" ~ "fm, passed checks",
    run_status_notes$fm_convergence == "TRUE" & run_status_notes$sigma_check != "go" & run_status_notes$censor_fm_check == "All Clear" ~ "fm, halted on checks (fail)",
    run_status_notes$fm_convergence == "TRUE" & run_status_notes$censor_fm_check == "BOTH" ~ "fm, halted on checks (fail)",
    run_status_notes$fm_convergence == "FALSE" & is.na(run_status_notes$censor_fm_check) ~ "nothing, fm failed",
    run_status_notes$fm_convergence == "FALSE" & run_status_notes$censor_fm_check == "BOTH" ~ "nothing, fm failed",
    run_status_notes$fm_convergence == "FALSE" & run_status_notes$censor_fm_check %in% c("LC", "RC") & fms_convergence == "FALSE" ~ "fm failed, tried fms, fms failed",
    run_status_notes$fm_convergence == "FALSE" & run_status_notes$censor_fm_check %in% c("LC", "RC") & fms_convergence == "TRUE" ~ "fm failed, tried fms, fms converged",
    run_status_notes$overall_censoring == "stop" ~ "nothing, halted on censoring level",
    TRUE ~ "unforeseen combo"
  )
  analysis <- case_when(
    length(results$single_model_output$single_model_output_fms) > 1 ~ "fms",
    length(results$single_model_output$single_model_output_fms) == 1 & length(results$single_model_output$single_model_output_fm) > 1 ~ "fm",
    TRUE ~ "nothing"
  )

  if(analysis == "fm"){
    single_model_output = results$single_model_output$single_model_output_fm
  } else if(analysis == "fms"){
    single_model_output = results$single_model_output$single_model_output_fms
  }else{
    single_model_output = "Error"
  }
  ##check directionality-----------


  if (analysis == "fm" & run_status_notes$fm_convergence == "TRUE") {
    directionality <- check_directionality(results, settings)

  } else{
    directionality = tibble(flip_decision = "not applicable", cross = "not applicable")
  }

  ##summary stats-------------
  if (directionality$flip_decision == "flip") {
    possible_data <-
      single_model_output$possible_data %>% rename("original_c" = c) %>% mutate(c = case_when(original_c == "1" ~ "2",
                                                                                              original_c == "2" ~ "1",
                                                                                              TRUE ~ "Error"))

  } else if (length(single_model_output) > 1) {
    possible_data <- single_model_output$possible_data

  } else{
    ###remake data set here
    set.seed(results$i)
    data.sim <- simulate_mics(
      n = settings$n,
      t_dist = settings$t_dist,
      pi = settings$pi,
      `E[X|T,C]` = settings$`E[X|T,C]`,
      sd_vector = settings$sd_vector,
      covariate_list = settings$covariate_list,
      covariate_effect_vector = settings$covariate_effect_vector,
      conc_limits_table = settings$conc_limits_table,
      low_con = settings$low_con,
      high_con = settings$high_con,
      scale = settings$scale
    ) %>% suppressMessages()
    visible_data <-
      prep_sim_data_for_em(
        data.sim,
        left_bound_name = "left_bound",
        right_bound_name = "right_bound",
        time = "t",
        covariate_names = settings$covariate_names,
        scale = settings$scale,
        keep_truth = settings$keep_true_values,
        observed_value_name = "observed_value",
        comp_name = "comp"
      )

    possible_data <-
      visible_data %>% #visible data with c for component
      #   group_by_all() %>%
      reframe(.by = everything(),
              c = as.character(1:2)
      )
  }



  ####PI Resid----------
  if (analysis %in% c("fm", "fms")) {
    pi_resid <-
      possible_data %>% filter(c == "2") %>% select(obs_id, t, comp) %>%
      mutate(pi_hat =
               case_when(
                 directionality %>% pull(flip_decision) == "flip" ~ 1 - (
                   predict(
                     single_model_output$binom_model,
                     data.frame(t = t),
                     type = "response"
                   )
                 ),
                 TRUE  ~ predict(
                   single_model_output$binom_model,
                   data.frame(t = t),
                   type = "response"
                 )
               )) %>%
      rowwise %>%
      mutate(pi_dgm = settings$pi(t) %>% pull(2)) %>%
      ungroup %>%
      mutate(false_resid = pi_dgm - pi_hat,
             resid = (comp == "2") * 1 - pi_hat) %>%
      summarise(
        #######NOT SURE WHAT TO DO HERE
        pi_resid_abs = mean(abs(resid)),
        pi_resid_sq = mean((resid) ^ 2),
        pi_false_resid_sq = mean((pi_dgm - pi_hat) ^ 2),
        pi_bias = mean(pi_dgm - pi_hat)
      )
  } else{
    pi_resid <-
      tibble(
        pi_resid_abs = NaN,
        pi_resid_sq = NaN,
        pi_false_resid_sq = NaN,
        pi_bias = NaN
      )
  }


  ###MU Resid-------
  if (analysis == "fm") {
    mu_resid_comp <-
      possible_data %>% filter(c == comp) %>%
      mutate(mu_dgm = settings$`E[X|T,C]`(t = t, c = comp)) %>%
      mutate(mu_hat = case_when(
        c == "1" & !is.na(single_model_output$newmodel[[1]]$scale) ~ predict(single_model_output$newmodel[[1]], data.frame(t = t)),
        c == "2" & !is.na(single_model_output$newmodel[[2]]$scale) ~ predict(single_model_output$newmodel[[2]], data.frame(t = t)),
        TRUE ~ NaN
      )) %>%
      mutate(resid = observed_value - mu_hat,
             false_resid = mu_dgm - mu_hat) %>%
      summarize(.by = comp,
                mu_resid_sq = mean((resid) ^ 2),
                mu_false_resid_sq = mean((mu_dgm - mu_hat) ^ 2),
                mu_bias = mean(mu_dgm - mu_hat)
      )  ###RUN AGAIN WITHOUT .by
    mu_resid_both <-
      possible_data %>% filter(c == comp) %>%
      mutate(mu_dgm = settings$`E[X|T,C]`(t = t, c = comp)) %>%
      mutate(mu_hat = case_when(
        c == "1" & !is.na(single_model_output$newmodel[[1]]$scale) ~ predict(single_model_output$newmodel[[1]], data.frame(t = t)),
        c == "2" & !is.na(single_model_output$newmodel[[1]]$scale) ~ predict(single_model_output$newmodel[[2]], data.frame(t = t)),
        TRUE ~ NaN
      )) %>%
      mutate(resid = observed_value - mu_hat,
             false_resid = mu_dgm - mu_hat) %>%
      summarize(
        mu_resid_sq = mean((resid) ^ 2),
        mu_false_resid_sq = mean((mu_dgm - mu_hat) ^ 2),
        mu_bias = mean(mu_dgm - mu_hat)
      ) %>%
      mutate(comp = "both")
    full_join(mu_resid_comp, tibble(comp = as.character(c(1,2)))) %>% suppressMessages() %>%

    rbind(., mu_resid_both) %>%
      pivot_wider(names_from = comp, values_from = mu_resid_sq:mu_bias) %>%
      select(mu_resid_sq_1,
             mu_resid_sq_2,
             mu_resid_sq_both,
             mu_false_resid_sq_1,
             mu_false_resid_sq_2,
             mu_false_resid_sq_both,
             mu_bias_1,
             mu_bias_2,
             mu_bias_both
      ) -> mu_resid

  } else if (analysis == "fms" & run_status_notes$censor_fm_check == "RC") {
    mu_resid <-
      possible_data %>% #filter(`P(C=c|y,t)` >= 0.5) %>%  ##Check, should it be the prediction for the predicted component? If we do this, need a way to resolve the exact 0.5s
      ##Other idea is to weight the residuals by `P(C=c|y,t)` instead of only choosing one
      ##Or do comp == c, so if the WT component gets pushed up, we know that is wrong and get huge residuals
      mutate(mu_dgm = settings$`E[X|T,C]`(t = t, c = comp)) %>%
      mutate(
        mu_hat = predict(single_model_output$newmodel, data.frame(t = t))) %>%  ###Should i filter after this to just get wt ones? but are we filtering by comp == 1 or c == 1) %>%
      filter(comp == 1) %>% #####QUESTIONABLE DECISION HERE
      mutate(resid = observed_value - mu_hat,
             false_resid = mu_dgm - mu_hat) %>%    ###SHOULD I GROUP BY COMP OR C?
      summarise(
        #######NOT SURE WHAT TO DO HERE
        mu_resid_sq_1 = mean((resid) ^ 2),
        mu_resid_sq_2 = NaN,
        mu_resid_sq_both = NaN,
        mu_false_resid_sq_1 = mean((mu_dgm - mu_hat) ^ 2),
        mu_false_resid_sq_2 = NaN,
        mu_false_resid_sq_both = NaN,
        mu_bias_1 = mean(mu_dgm - mu_hat),
        mu_bias_2 = NaN,
        mu_bias_both = NaN
      )
  } else if (analysis == "fms" & run_status_notes$censor_fm_check == "LC") {
    mu_resid <-
      possible_data %>% #filter(`P(C=c|y,t)` >= 0.5) %>%  ##Check, should it be the prediction for the predicted component? If we do this, need a way to resolve the exact 0.5s
      ##Other idea is to weight the residuals by `P(C=c|y,t)` instead of only choosing one
      ##Or do comp == c, so if the WT component gets pushed up, we know that is wrong and get huge residuals
      mutate(mu_dgm = settings$`E[X|T,C]`(t = t, c = comp)) %>%
      mutate(
        mu_hat = predict(single_model_output$newmodel, data.frame(t = t))) %>%  ###Should i filter after this to just get wt ones? but are we filtering by comp == 1 or c == 1) %>%
      filter(comp == 2) %>% #####QUESTIONABLE DECISION HERE
      mutate(resid = observed_value - mu_hat,
             false_resid = mu_dgm - mu_hat) %>%    ###SHOULD I GROUP BY COMP OR C?
      summarise(
        #######NOT SURE WHAT TO DO HERE
        mu_resid_sq_1 = NaN,
        mu_resid_sq_2 = mean((resid) ^ 2),
        mu_resid_sq_both = NaN,
        mu_false_resid_sq_1 = NaN,
        mu_false_resid_sq_2 = mean((mu_dgm - mu_hat) ^ 2),
        mu_false_resid_sq_both = NaN,
        mu_bias_1 = NaN,
        mu_bias_2 = mean(mu_dgm - mu_hat),
        mu_bias_both = NaN
      )
  }else{
    mu_resid <- tibble(
      mu_resid_sq_1 = NaN,
      mu_resid_sq_2 = NaN,
      mu_resid_sq_both = NaN,
      mu_false_resid_sq_1 = NaN,
      mu_false_resid_sq_2 = NaN,
      mu_false_resid_sq_both = NaN,
      mu_bias_1 = NaN,
      mu_bias_2 = NaN,
      mu_bias_both = NaN
    )
  }



  comp_scales <- scale_select(results, directionality, analysis, single_model_output)

  ###Time for area calculation stuff

  ###CREATE VERSIONS OF THESE WITH THE INTEGRATE FUNCTION



  mu_area <- calculate_mu_area(results, settings, directionality, analysis)




  ##NEEDS FLIP COMPATIBILITY

  pi_area <- calculate_pi_area(results, settings, directionality, analysis, single_model_output)



  ##censoring info
  ##get settings also, grab scale so log has -inf and mic has 0 for left bound for left censored


  # full_join(
  #   censoring_post_info(possible_data, setting, comparison = "model_weighted"),
  #   censoring_post_info(possible_data, setting, comparison = "true_pct"),
  #   by = c("comp", "censor")
  # )

  if (length(single_model_output) > 1) {
    censoring_levels <- cbind(
      censoring_post_info(possible_data, settings, comparison = "model_weighted") %>%
        full_join(., tibble(comp = as.character(c(rep(1, 3), rep(2, 3))), censor = rep(c("left", "interval", "right"),2))) %>%
        suppressMessages() %>%
        pivot_wider(
          names_from = c(comp, censor),
          values_from = weighted_prop_model,
          names_prefix = "model_cens_"
        ),
      censoring_post_info(possible_data, settings, comparison = "true_pct") %>%
        full_join(., tibble(comp = as.character(c(rep(1, 3), rep(2, 3))), censor = rep(c("left", "interval", "right"),2))) %>%
        suppressMessages() %>%
        pivot_wider(
          names_from = c(comp, censor),
          values_from = weighted_prop_true,
          names_prefix = "true_cens_"
        )
    ) %>% tibble() %>%
      suppressMessages()
  } else{
    censoring_levels <-
      tibble(
        model_cens_1_interval = NaN,
        model_cens_1_left = NaN,
        model_cens_1_right = NaN,
        model_cens_2_interval = NaN,
        model_cens_2_left = NaN,
        model_cens_2_right = NaN,
        censoring_post_info(possible_data, settings, comparison = "true_pct") %>%
          full_join(., tibble(comp = as.character(c(rep(1, 3), rep(2, 3))), censor = rep(c("left", "interval", "right"),2))) %>%
          suppressMessages() %>%
          pivot_wider(
            names_from = c(comp, censor),
            values_from = weighted_prop_true,
            names_prefix = "true_cens_"
          )
      )
  }

  #scenario <- case_when(
  #  (
  #    results$failure_safety_notes["fm_fail"] == "fm_worked" &
  #      !(results$failure_safety_notes["fms_only"] %>% as.logical())
  #  ) ~ "fm",
  #  results$failure_safety_notes["fms_fail"] == "fms_worked" ~ "fms",
  #  results$failure_safety_notes["fms_fail"] == "fms_failed" ~ "fms_failed",
  #  (results$failure_safety_notes["fm_fail"] %in% c("fm_failed", "fm_failed_cutoff")) &  results$failure_safety_notes["fms_fail"] == "fms_not_allowed" & !(results$failure_safety_notes["allow_safety"] %>% as.logical()) ~ "fm_failed, no fms",
  #  results$failure_safety_notes["fm_fail"] == "fm_worked" & (results$failure_safety_notes["fms_only"] %>% as.logical()) ~ "fm_worked, not saved",
  #  TRUE ~ "other"
  #
  #)

  ##Bind output into a single tibble (could do 1 row or multiple rows, just add i)
  cbind(situation, analysis, mu_resid, pi_resid, comp_scales, censoring_levels, results$run_status_notes %>% t() %>% as_tibble) %>%
    tibble %>%
    mutate(flip = directionality %>%
             pull(flip_decision),
           cross = directionality %>%
             pull(cross),
           steps = ifelse(
             length(single_model_output) == 1,
             NaN,
             single_model_output$steps
           )) %>%
    mutate(iteration = results$i) %>%
    select(iteration, flip, cross, steps, everything()) %>%
    tibble(., mu_area, pi_area) %>%
    return()

}
