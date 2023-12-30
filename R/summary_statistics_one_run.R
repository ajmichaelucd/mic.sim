#' Title
#'
#' @param run_number
#' @param files_folder
#' @param run_name
#' @param n_setups
#' @param n_runs_per_setup
#' @param date
#'
#' @return
#' @export
#'
#' @examples
summary_statistics_one_run = function(run_number, files_folder, run_name, n_setups, n_runs_per_setup, date){

  setup_number = (run_number / n_runs_per_setup) %>% ceiling()

  file_name = paste0(files_folder, run_name, "_row_", setup_number, "_", date,"_run_", run_number, ".Rdata")

  batch = loadRData(file_name)

  #batch$batch_parameters
  #batch$batch_output
  map(batch$batch_output, ~summary_statistics_one_data_set(.x, batch$batch_parameters)) %>%
    data.table::rbindlist() %>% tibble %>%
    mutate(row = setup_number,
           run = run_number) %>% return()

}


summary_statistics_one_data_set = function(one_set_output, parameters){
  map(one_set_output$set_output, ~summary_statistcs_one_model(.x, one_set_output, parameters)) %>%
    data.table::rbindlist(use.names=TRUE) %>%
    tibble %>% return()
}

summary_statistcs_one_model = function(model_output, one_set_output, parameters){

  single_model_output = model_output$output
  ##Error
  ##One of two comps converge
  ##Both comps converge
  model_output$final_like$converge
  model_output$final_like$comp_conv

  if(model_output$final_like$comp_conv == "both"){
    c1_scale_est = single_model_output$mu_model[[1]]$scale
    c2_scale_est = single_model_output$mu_model[[2]]$scale

    sq_resid_weight = single_model_output$possible_data %>% mutate(predicted_mu = case_when(
      c == 1 ~ predict(single_model_output$mu_model[[1]], newdata = data.frame(t = t)),
      c == 2 ~ predict(single_model_output$mu_model[[2]], newdata = data.frame(t = t)),
      TRUE ~ NA_integer_
    ),
    resid = ((observed_value - predicted_mu) * `P(C=c|y,t)`)^2 ##DO I ABS VALUE, SUM, THEN SQUARE OR SQUARE THEN SUM? I'M GOING TO SQUARE FIRST FOR NOW
    ) %>% summarize(sq_resid_weight = sum(resid)) %>% pull

    resid_tibble_comp = single_model_output$possible_data %>% filter(c == comp) %>% mutate(predicted_mu = case_when(
      c == 1 ~ predict(single_model_output$mu_model[[1]], newdata = data.frame(t = t)),
      c == 2 ~ predict(single_model_output$mu_model[[2]], newdata = data.frame(t = t)),
      TRUE ~ NA_integer_
    ),
    resid = (observed_value - predicted_mu)) %>%
      summarize(.by = c,
                bias = mean(resid),
                sq_res_avg = mean((observed_value - predicted_mu)^2),
                sq_res_med =  median((observed_value - predicted_mu)^2)
      ) %>% right_join(., tibble(c = c("1", "2")), by = "c") %>% pivot_wider(names_from = c, values_from = bias:sq_res_med) %>% select(bias_1, bias_2, sq_res_avg_1, sq_res_avg_2, sq_res_med_1, sq_res_med_2)

    pi_metrics = single_model_output$possible_data %>% filter(c == "2") %>% select(obs_id, t, comp) %>%
      mutate(pi_hat =
               predict(
                 single_model_output$pi_model,
                 data.frame(t = t),
                 type = "response"
               )
      ) %>%
      mutate(.by = obs_id,pi_dgm = parameters$pi(t) %>% pull(2)) %>%
      mutate(false_resid = pi_dgm - pi_hat,
             resid = (comp == "2") * 1 - pi_hat) %>%
      summarise(
        #######NOT SURE WHAT TO DO HERE
        pi_resid_abs = mean(abs(resid)),
        pi_resid_sq = mean((resid) ^ 2),
        pi_false_resid_sq = mean((pi_dgm - pi_hat) ^ 2),
        pi_bias = mean(pi_dgm - pi_hat)
      )


  }else if(model_output$final_like$comp_conv == "comp 1"){
    c1_scale_est = single_model_output$mu_model[[1]]$scale
    c2_scale_est = NA_integer_
    sq_resid_weight = NA_integer_
    resid_tibble_comp = single_model_output$possible_data %>% filter(c == comp) %>% mutate(predicted_mu = case_when(
      c == 1 ~ predict(single_model_output$mu_model[[1]], newdata = data.frame(t = t)),
      TRUE ~ NA_integer_
    ),
    resid = (observed_value - predicted_mu)) %>%
      summarize(.by = c,
                bias = mean(resid),
                sq_res_avg = mean((observed_value - predicted_mu)^2),
                sq_res_med =  median((observed_value - predicted_mu)^2)
      ) %>% right_join(., tibble(c = c("1", "2")), by = "c") %>% pivot_wider(names_from = c, values_from = bias:sq_res_med) %>% select(bias_1, bias_2, sq_res_avg_1, sq_res_avg_2, sq_res_med_1, sq_res_med_2)

    pi_metrics = single_model_output$possible_data %>% filter(c == "2") %>% select(obs_id, t, comp) %>%
      mutate(pi_hat =
               predict(
                 single_model_output$pi_model,
                 data.frame(t = t),
                 type = "response"
               )
      ) %>%
      mutate(.by = obs_id,pi_dgm = parameters$pi(t) %>% pull(2)) %>%
      mutate(false_resid = pi_dgm - pi_hat,
             resid = (comp == "2") * 1 - pi_hat) %>%
      summarise(
        #######NOT SURE WHAT TO DO HERE
        pi_resid_abs = mean(abs(resid)),
        pi_resid_sq = mean((resid) ^ 2),
        pi_false_resid_sq = mean((pi_dgm - pi_hat) ^ 2),
        pi_bias = mean(pi_dgm - pi_hat)
      )

  }else if(model_output$final_like$comp_conv == "comp 2"){
    c1_scale_est = NA_integer_
    c2_scale_est = single_model_output$mu_model[[2]]$scale
    sq_resid_weight = NA_integer_
    resid_tibble_comp = single_model_output$possible_data %>% filter(c == comp) %>% mutate(predicted_mu = case_when(
      c == 2 ~ predict(single_model_output$mu_model[[2]], newdata = data.frame(t = t)),
      TRUE ~ NA_integer_
    ),
    resid = (observed_value - predicted_mu)) %>%
      summarize(.by = c,
                bias = mean(resid),
                sq_res_avg = mean((observed_value - predicted_mu)^2),
                sq_res_med =  median((observed_value - predicted_mu)^2)
      ) %>% right_join(., tibble(c = c("1", "2")), by = "c") %>%  pivot_wider(names_from = c, values_from = bias:sq_res_med) %>% select(bias_1, bias_2, sq_res_avg_1, sq_res_avg_2, sq_res_med_1, sq_res_med_2)

    pi_metrics = single_model_output$possible_data %>% filter(c == "2") %>% select(obs_id, t, comp) %>%
      mutate(pi_hat =
               predict(
                 single_model_output$pi_model,
                 data.frame(t = t),
                 type = "response"
               )
      ) %>%
      mutate(.by = obs_id,pi_dgm = parameters$pi(t) %>% pull(2)) %>%
      mutate(false_resid = pi_dgm - pi_hat,
             resid = (comp == "2") * 1 - pi_hat) %>%
      summarise(
        #######NOT SURE WHAT TO DO HERE
        pi_resid_abs = mean(abs(resid)),
        pi_resid_sq = mean((resid) ^ 2),
        pi_false_resid_sq = mean((pi_dgm - pi_hat) ^ 2),
        pi_bias = mean(pi_dgm - pi_hat)
      )

  }else{
    c1_scale_est = NA_integer_
    c2_scale_est = NA_integer_
    sq_resid_weight = NA_integer_

    resid_tibble_comp = tibble(bias_1 = NA_integer_, bias_2 = NA_integer_, sq_res_avg_1 = NA_integer_, sq_res_avg_2 = NA_integer_, sq_res_med_1 = NA_integer_, sq_res_med_2 = NA_integer_,)

    pi_metrics = tibble(pi_resid_abs = NA_integer_, pi_resid_sq = NA_integer_, pi_false_resid_sq = NA_integer_, pi_bias = NA_integer_)
  }



  tibble(
    mic_seed = one_set_output$set_mic_seed,
    model_output$final_like,
    c1_scale_est = c1_scale_est,
    c2_scale_est = c2_scale_est,
    c1_scale_truth = parameters$sd_vector["1"],
    c2_scale_truth = parameters$sd_vector["2"],
    sq_resid_weight = sq_resid_weight,
    resid_tibble_comp,
    integrate_difference_error(model_output = model_output, parameters = parameters)

  ) %>% return()



}

integrate_difference_error = function(model_output, parameters, t_min = 0, t_max = 16){
  single_model_output = model_output$output

  if(model_output$final_like$comp_conv %in% c("both", "comp 1")){
    area_1 = integrate(function(t){abs(predict(single_model_output$mu_model[[1]], newdata = data.frame(t = t)) -
        parameters$`E[X|T,C]`(t, "1"))}, lower = t_min, upper = t_max)
  }else{
    area_1 = list(value = NA_integer_)
  }
  if(model_output$final_like$comp_conv %in% c("both", "comp 2")){
    area_2 = integrate(function(t){abs(predict(single_model_output$mu_model[[2]], newdata = data.frame(t = t)) -
        parameters$`E[X|T,C]`(t, "2"))}, lower = t_min, upper = t_max)
  }else{
    area_2 = list(value = NA_integer_)
  }

  if(model_output$final_like$comp_conv != "neither"){
    area_3 = integrate(function(t){abs(predict(single_model_output$pi_model, newdata = data.frame(t = t), type = "response") -
        parameters$pi(t) %>% pull(2))}, lower = t_min, upper = t_max)
  }else{
    area_3 = list(value = NA_integer_)
  }
  return(tibble(
    abs_error_area_mu_1 = area_1$value,
    abs_error_area_mu_2 = area_2$value,
    abs_error_area_pi = area_3$value
  ))
}


