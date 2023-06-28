library(tidyr)
#new approach to analysis:

#load in 1 batch at a time (map over this)

#for each iteration in the 10, collect:
  #residuals for pi (both types)
  #residuals for mu (by component) (both types)
  #oracle vs fitted model mu residuals-like thing
  #pct of obs classified correctly
  #area btwn mu_fitted and dgm
  #area btwn pi_fitted and dgm

###Parameters to estimate and other simulation info----------------------
#number of batches (e.g. 100)
number_of_batches = 100
#number per batch (e.g. 10)
number_per_batch = 10
#check by putting total number here
number_of_iterations = 1000

#path to the directory full of the files
location <- "~/Desktop/june_2023/run_form2_loess_2"


#two formats i have used:
#name_date_number
#name_number_date
format <- "name_date_number"

#general name of simulation array
array_name <- "run_form2_loess_2"
date <- "06132023"

incomplete <- check_array_complete(number_of_batches = number_of_batches, format = format, location = location, array_name = array_name, date = date)

print(incomplete)

##if we have an incomplete array, let's do reruns of those batches locally
if(nrow(tibble(incomplete)) > 1 |  (nrow(tibble(incomplete))  == 1 && incomplete != "All Clear")){
  ###start by getting the parameters to do reruns if needed
    ##select one of the batches that did run to grab the parameters
i = tibble(complete = 1:number_of_batches) %>% filter(!complete %in% (incomplete %>% pull)) %>% pull %>% sample(., size = 1, replace = FALSE)
      #load in that completed batch
file  <- gen_path_sim(location = location, format = format, array_name = array_name, date = date, i = i)
batch_results <- loadRData(file)

      #save the settings from the batch to use for reruns of other batches
rerun_parameters <- batch_results$settings

##grabbed the parameters, now to rerun these locally:
 ######NEEDS ADDITION HERE
##function to map over all batches
    ##function to map over all iterations
        ##function to fit locally using parameters provided from above passed through the other two functions (except for $iteration_set)


}


incomplete <- check_array_complete(number_of_batches = number_of_batches, format = format, location = location, array_name = array_name, date = date)

print(incomplete)

##map over batches a function to load in a batch
    ##map over the iterations of a batch a function to analyze an iteration


capture_error_measures_one_batch_2.0 <- function(location,
                                             format,
                                             array_name,
                                             date,
                                             i,
                                             batch_size,
                                             number_of_batches
){
###Load in a batch--------------------------
  file  <- gen_path_sim(location = location, format = format, array_name = array_name, date = date, i = i)
  batch_results <- loadRData(file)



###Visualize Progress-----------------------
  hits = 1:100
  scaled = number_of_batches/100
  reports = round(scaled * hits)



  if(i %in% reports){
    a <- paste(rep("|", round(i / scaled)), collapse = "")
    b <- paste(rep(".", 100 - round(i/scaled)), collapse = "")

    paste0(paste0(a, b), "*") %>% print()
  }


###Analysis of iterations---------------------
  purrr::map(batch_results$model_results, ~capture_error_measures_one_run_2.0(results =  .x, settings = batch_results$settings)) %>%
    data.table::rbindlist()
  ##add attribute read to the error_measures_one_run_both_directions segment

}

capture_error_measures_one_run_2.0(results, settings){
  #first: Error? If so note this and pass this to end
    #If not, are we looking at the findings of fit_model_pi or fit_model_safety_pi
      #function for fit_model_pi results
        ###first identify wt vs nonwt
      #function for fit_model_safety_pi results
}


#work on fit_model_pi results-----------------

get_t_min_max <- function(settings){
m <- functionBody(settings$t_dist) %>% as.character() %>% stringr::str_match(., "min =\\s*(.*?)\\s*, max =")

t_min <- m[2,2] %>% as.numeric()


g <- functionBody(settings$t_dist) %>% as.character() %>% stringr::str_split(., pattern = "max =", simplify = TRUE) %>% as.matrix() #%>% parse_number()

t_max <- g[2,2] %>% parse_number()

tibble(t_min, t_max) %>% return()
}


##check directionality-----------


if (results$failure_safety_notes["fm_fail"] == "fm_worked" &
    !(results$failure_safety_notes["fms_only"] %>% as.logical())) {
  check_directionality <-
    function(results = results,
             settings = settings) {
      df <-
        tibble(t = seq(
          get_t_min_max(settings = settings) %>% pull(t_min),
          get_t_min_max(settings = settings) %>% pull(t_max),
          length.out = 1000
        )) %>%
        mutate(
          c1 = predict(results$single_model_output$newmodel[[1]], newdata = tibble(t)),
          c2 = predict(results$single_model_output$newmodel[[2]], newdata = tibble(t)),
          flip = case_when(c1 > c2 ~ "flip",
                           c2 > c1 ~ "no flip",
                           c2 == c1 ~ "equal",
                           TRUE ~ NA)
        ) %>%
        group_by(flip) %>%
        summarise(n = n())

      if (nrow(df) == 1) {
        flip_decision <- df %>% pull(flip)
        cross <- "no cross"
        directionality <- df %>%
          pivot_wider(names_from = flip, values_from = n) %>%
          mutate(flip_decision = flip_decision,
                 cross = cross)

      } else{
        directionality <-
          df %>%
          pivot_wider(names_from = flip, values_from = n) %>%
          mutate(
            flip_decision = case_when(
              flip > `no flip` ~ "flip",
              flip <= `no flip` ~ "no flip",
              TRUE ~ "Error"
            ),
            cross = "cross"
          )
      }
      return(directionality)
    }

  directionality <- check_directionality(results, settings)

} else{
  directionality = tibble(flip_decision = "not applicable", cross = "not applicable")
}

##summary stats-------------
if (directionality$flip_decision == "flip") {
  possible_data <-
    results$single_model_output$possible_data %>% rename("original_c" = c) %>% mutate(c = case_when(original_c == "1" ~ "2",
                                                                                                    original_c == "2" ~ "1",
                                                                                                    TRUE ~ "Error"))

} else if (length(results$single_model_output) > 1) {
  possible_data <- results$single_model_output$possible_data

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
  )
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
if ((results$failure_safety_notes["fm_fail"] == "fm_worked" &
    !(results$failure_safety_notes["fms_only"] %>% as.logical())) |
    results$failure_safety_notes["fms_fail"] == "fms_worked") {
  pi_resid <-
    possible_data %>% filter(c == "2") %>% select(obs_id, t, comp) %>%
    mutate(pi_hat =
             case_when(
               directionality %>% pull(flip_decision) == "flip" ~ 1 - (
                 predict(
                   results$single_model_output$binom_model,
                   data.frame(t = t),
                   type = "response"
                 )
               ),
               TRUE  ~ predict(
                 results$single_model_output$binom_model,
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
      pi_resid_abs = sum(abs(resid)),
      pi_resid_sq = sum((resid) ^ 2),
      pi_false_resid_sq = sum((pi_dgm - pi_hat) ^ 2),
      pi_bias = sum(pi_dgm - pi_hat)
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
if (results$failure_safety_notes["fm_fail"] == "fm_worked" &
    !(results$failure_safety_notes["fms_only"] %>% as.logical())) {
  mu_resid <-
    possible_data %>% filter(`P(C=c|y,t)` >= 0.5) %>%  ##Check, should it be the prediction for the predicted component? If we do this, need a way to resolve the exact 0.5s
    ##Other idea is to weight the residuals by `P(C=c|y,t)` instead of only choosing one
    ##Or do comp == c, so if the WT component gets pushed up, we know that is wrong and get huge residuals
    mutate(mu_dgm = settings$`E[X|T,C]`(t = t, c = comp)) %>%
    mutate(mu_hat = case_when(
      c == "1" ~ predict(results$single_model_output$newmodel[[1]], data.frame(t = t)),
      c == "2" ~ predict(results$single_model_output$newmodel[[2]], data.frame(t = t)),
      TRUE ~ NaN
    )) %>%
    mutate(resid = observed_value - mu_hat,
           false_resid = mu_dgm - mu_hat) %>%    ###SHOULD I GROUP BY COMP OR C?
    summarise(
      #######NOT SURE WHAT TO DO HERE
      mu_resid_sq = sum((resid) ^ 2),
      mu_false_resid_sq = sum((mu_dgm - mu_hat) ^ 2),
      mu_bias = sum(mu_dgm - mu_hat)
    )
} else if (results$failure_safety_notes["fms_fail"] == "fms_worked") {
  mu_resid <-
    possible_data %>% filter(`P(C=c|y,t)` >= 0.5) %>%  ##Check, should it be the prediction for the predicted component? If we do this, need a way to resolve the exact 0.5s
    ##Other idea is to weight the residuals by `P(C=c|y,t)` instead of only choosing one
    ##Or do comp == c, so if the WT component gets pushed up, we know that is wrong and get huge residuals
    mutate(mu_dgm = settings$`E[X|T,C]`(t = t, c = comp)) %>%
    mutate(
      mu_hat = predict(results$single_model_output$newmodel, data.frame(t = t))) %>%  ###Should i filter after this to just get wt ones? but are we filtering by comp == 1 or c == 1) %>%
      filter(comp == 1) %>% #####QUESTIONABLE DECISION HERE
        mutate(resid = observed_value - mu_hat,
               false_resid = mu_dgm - mu_hat) %>%    ###SHOULD I GROUP BY COMP OR C?
        summarise(
          #######NOT SURE WHAT TO DO HERE
          mu_resid_sq = sum((resid) ^ 2),
          mu_false_resid_sq = sum((mu_dgm - mu_hat) ^ 2),
          mu_bias = sum(mu_dgm - mu_hat)
        )
} else{
  mu_resid <- tibble(
    mu_resid_sq = NaN,
    mu_false_resid_sq = NaN,
    mu_bias = NaN
  )
}







scale_select <- function(results, directionality) {
  if (results$failure_safety_notes["fm_fail"] == "fm_worked" &
      !(results$failure_safety_notes["fms_only"] %>% as.logical())) {
    if (directionality %>% pull(flip_decision) == "no flip") {
      tibble(
        c1_scale = results$single_model_output$newmodel[[1]]$scale,
        c2_scale = results$single_model_output$newmodel[[2]]$scale
      ) %>% return()
    } else{
      tibble(
        c1_scale = results$single_model_output$newmodel[[2]]$scale,
        c2_scale = results$single_model_output$newmodel[[1]]$scale
      ) %>% return()
    }
  } else if (results$failure_safety_notes["fms_fail"] == "fms_worked") {
    tibble(c1_scale = results$single_model_output$newmodel$scale,
           c2_scale = NaN) %>% return()
  } else{
    scale_select <- tibble(c1_scale = NaN,
                           c2_scale = NaN)
  }
}




comp_scales <- scale_select(results, directionality)

###Time for area calculation stuff



calculate_mu_area <- function(results, settings, directionality) {
  if (results$failure_safety_notes["fm_fail"] == "fm_worked" &
      !(results$failure_safety_notes["fms_only"] %>% as.logical())) {
    tibble(
      t = seq(
        get_t_min_max(settings = settings) %>% pull(t_min),
        get_t_min_max(settings = settings) %>% pull(t_max),
        length.out = 100000
      ),
      width = (
        get_t_min_max(settings = settings) %>% pull(t_max) - get_t_min_max(settings = settings) %>% pull(t_min)
      ) / 100000
    ) %>%
      reframe(.by = everything(),    #implement for other initial weighting options too ##########
              c = as.character(1:2)) %>%
      mutate(
        mu_hat = case_when(
          (directionality %>% pull(flip_decision) == "no flip") &
            c == "1" ~ predict(results$single_model_output$newmodel[[1]], data.frame(t = t)),
          (directionality %>% pull(flip_decision) == "no flip") &
            c == "2" ~ predict(results$single_model_output$newmodel[[2]], data.frame(t = t)),
          (directionality %>% pull(flip_decision) == "flip") &
            c == "1" ~ predict(results$single_model_output$newmodel[[2]], data.frame(t = t)),
          (directionality %>% pull(flip_decision) == "flip") &
            c == "2" ~ predict(results$single_model_output$newmodel[[1]], data.frame(t = t)),
          TRUE ~ NaN
        ),
        mu_dgm = settings$`E[X|T,C]`(t = t, c = c),
        diff = mu_dgm - mu_hat,
        area = abs(diff) * width
      ) %>%
      group_by(c) %>%
      summarise(
        total_area = sum(area),
        avg_diff = mean(diff),
        med_diff = median(diff)
      ) %>%
      pivot_wider(
        names_from = c,
        names_prefix = "c",
        values_from = total_area:med_diff
      ) %>% return()
  } else if (results$failure_safety_notes["fms_fail"] == "fms_worked") {
    tibble(
      t = seq(
        get_t_min_max(settings = settings) %>% pull(t_min),
        get_t_min_max(settings = settings) %>% pull(t_max),
        length.out = 100000
      ),
      width = (
        get_t_min_max(settings = settings) %>% pull(t_max) - get_t_min_max(settings = settings) %>% pull(t_min)
      ) / 100000
    ) %>%
      reframe(.by = everything(),    #implement for other initial weighting options too ##########
              c = as.character(1:2)) %>%
      mutate(
        mu_hat = case_when(
          c == "1" ~ predict(results$single_model_output$newmodel, data.frame(t = t)),
          c == "2" ~ NaN,
          TRUE ~ NaN
        ),
        mu_dgm = settings$`E[X|T,C]`(t = t, c = c),
        diff = mu_dgm - mu_hat,
        area = abs(diff) * width
      ) %>%
      group_by(c) %>%
      summarise(
        total_area = sum(area),
        avg_diff = mean(diff),
        med_diff = median(diff)
      ) %>%
      pivot_wider(
        names_from = c,
        names_prefix = "c",
        values_from = total_area:med_diff
      ) %>% return()
  } else{
    tibble(
      total_area_c1 = NaN,
      total_area_c2 = NaN,
      avg_diff_c1 = NaN,
      avg_diff_c2 = NaN,
      med_diff_c1 = NaN,
      med_diff_c2 = NaN
    ) %>% return()
  }

}

mu_area <- calculate_mu_area(results, settings, directionality)




calculate_pi_area <- function(results, settings, directionality) {
  if ((results$failure_safety_notes["fm_fail"] == "fm_worked" &
       !(results$failure_safety_notes["fms_only"] %>% as.logical())) |
      results$failure_safety_notes["fms_fail"] == "fms_worked") {
    tibble(
    t = seq(
      get_t_min_max(settings = settings) %>% pull(t_min),
      get_t_min_max(settings = settings) %>% pull(t_max),
      length.out = 100000
    ),
    width = (
      get_t_min_max(settings = settings) %>% pull(t_max) - get_t_min_max(settings = settings) %>% pull(t_min)
    ) / 100000
  ) %>%
    mutate(
      pi_hat =
        case_when(
          directionality %>% pull(flip_decision) == "flip" ~ 1 - predict(
            results$single_model_output$binom_model,
            data.frame(t = t),
            type = "response"
          ),
          TRUE ~ predict(
            results$single_model_output$binom_model,
            data.frame(t = t),
            type = "response"
          )
        ),
      pi_dgm = settings$pi(t) %>% pull("2"),
      diff = pi_dgm - pi_hat,
      area = abs(diff) * width
    ) %>%
    summarise(
      total_area_pi = sum(area),
      avg_diff_pi = mean(diff),
      med_diff_pi = median(diff)
    ) %>% suppressWarnings()
  }else{
    tibble(total_area_pi = NaN,
           avg_diff_pi = NaN,
           med_diff_pi = NaN)
  }

} ##NEEDS FLIP COMPATIBILITY

pi_area <- calculate_pi_area(results, settings, directionality)



##censoring info
    ##get settings also, grab scale so log has -inf and mic has 0 for left bound for left censored
censoring_post_info <-
  function(possible_data, setting, comparison = "model_weighted") {
    df <- possible_data %>%
      mutate(
        censor =
          case_when(
            settings$scale == "log" & left_bound == -Inf ~ "left",
            settings$scale == "MIC" &
              (left_bound == 0 | left_bound == -Inf) ~ "left",
            right_bound == Inf ~ "right",
            TRUE ~ "interval"
          )
      )

    if (comparison == "model_weighted") {
      df %>% group_by(c, censor) %>% summarise(p = sum(`P(C=c|y,t)`)) %>%
        mutate(sum = sum(p[c == c]),
               weighted_prop_model = p / sum) %>%
        ungroup() %>%
        select(-c(p, sum)) %>%
        rename(comp = c) %>%
        tidyr::complete(comp, censor, fill = list(weighted_prop_model = 0)) %>%
        return()

    } else if (comparison == "true_pct") {
      df %>% group_by(comp, censor) %>% summarise(p = n()) %>%
        mutate(sum = sum(p[comp == comp]),
               weighted_prop_true = p / sum) %>%
        ungroup() %>%
        select(-c(p, sum)) %>%
        tidyr::complete(comp, censor, fill = list(weighted_prop_true = 0)) %>%
        return()

    } else{
      errorCondition("choose model_weighted or true_pct")
    }

  }

# full_join(
#   censoring_post_info(possible_data, setting, comparison = "model_weighted"),
#   censoring_post_info(possible_data, setting, comparison = "true_pct"),
#   by = c("comp", "censor")
# )

if (length(results$single_model_output) > 1) {
  censoring_levels <- cbind(
    censoring_post_info(possible_data, setting, comparison = "model_weighted") %>%
      pivot_wider(
        names_from = c(comp, censor),
        values_from = weighted_prop_model,
        names_prefix = "model_cens_"
      ),
    censoring_post_info(possible_data, setting, comparison = "true_pct") %>%
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
      censoring_post_info(possible_data, setting, comparison = "true_pct") %>%
        pivot_wider(
          names_from = c(comp, censor),
          values_from = weighted_prop_true,
          names_prefix = "true_cens_"
        )
    )
}

scenario <- case_when(
  (
    results$failure_safety_notes["fm_fail"] == "fm_worked" &
      !(results$failure_safety_notes["fms_only"] %>% as.logical())
  ) ~ "fm",
  results$failure_safety_notes["fms_fail"] == "fms_worked" ~ "fms",
  results$failure_safety_notes["fms_fail"] == "fms_failed" ~ "fms_failed",
  (results$failure_safety_notes["fm_fail"] %in% c("fm_failed", "fm_failed_cutoff")) &  results$failure_safety_notes["fms_fail"] == "fms_not_allowed" & !(results$failure_safety_notes["allow_safety"] %>% as.logical()) ~ "fm_failed, no fms",
  results$failure_safety_notes["fm_fail"] == "fm_worked" & (results$failure_safety_notes["fms_only"] %>% as.logical()) ~ "fm_worked, not saved",
  TRUE ~ "other"

)

##Bind output into a single tibble (could do 1 row or multiple rows, just add i)
cbind(scenario, mu_resid, pi_resid, comp_scales, censoring_levels) %>%
  tibble %>%
  mutate(flip = directionality %>%
           pull(flip_decision),
         cross = directionality %>%
           pull(cross)) %>%
  mutate(iteration = results$i) %>%
  select(iteration, flip, cross, everything()) %>%
  tibble(., mu_area, pi_area)



results$failure_safety_notes



