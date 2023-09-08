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

##START--------


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

if (prelim_cens_check %>% filter(cens != "interval_censored") %>% pull(proportion) %>% sum >= max_cens_tolerance) {
  overall_censoring = "stop"
}else{
  overall_censoring = "go"

get_1_comp_model = function(visible_data, formula, max_it, tol_ll, verbose, maxiter_survreg){
fm_output_1 = fit_model_pi(visible_data = visible_data,
              formula = formula,
              formula2 = NULL,
              max_it = max_it,
             ncomp = 1,
              tol_ll = tol_ll,
              pi_link = NULL,
              verbose = verbose,
              maxiter_survreg = maxiter_survreg,
              initial_weighting = NULL)
if (length(fm_output_1) == 1) {
  fm_convergence = FALSE
}else{
  fm_convergence = TRUE
}
run_status_notes =
  c(fm_convergence = fm_convergence)
output_1 = list(
  list(single_model_output_fm = fm_output_1),
  run_status_notes = run_status_notes)
return(output_1)
}
output_1 = get_1_comp_model(visible_data, formula, max_it, tol_ll, verbose, maxiter_survreg)


##function for 2 comp here
#poss_fit_model <-
#  purrr::possibly(.f = fit_model_pi, otherwise = "Error")

#list of outputs: fm_output_2
                  #fm_convergence
                  #sigma_check
                  #censor_fm_check
                  #fms_convergence (first as NA vs tbd, then as NA, TRUE, FALSE)
                  #single_model_output_fms
get_2_comp_model = function(visible_data, formula, formula2, max_it, tol_ll, pi_link, verbose, maxiter_survreg, initial_weighting){
single_model_output_fm = fit_model_pi(
             visible_data = visible_data,
             formula = formula,
             formula2 = formula2,
             max_it = max_it,
             ncomp = 2,
             tol_ll = tol_ll,
             pi_link = pi_link,
             verbose = verbose,
             maxiter_survreg = maxiter_survreg,
             initial_weighting = initial_weighting)
if (length(single_model_output_fm) == 1) {
fm_convergence = FALSE
}else{
  fm_convergence = case_when(single_model_output_fm$converge %in% c("YES", "iterations")  ~ TRUE,
                             TRUE ~ FALSE)
  #model converged, so let's check the fit
}

if(fm_convergence){
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
if(length(single_model_output_fm) > 1 & !fm_convergence){ ##put the stuff for a model that didn't converge but generated output here, assign censor_fm_check as "RC", "LC", or both? skip sigma check

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
if(length(single_model_output_fm) == 1){
  sigma_check = NA_character_
  censor_fm_check = NA_character_
  fms_convergence = "PASS"
}

if(!is.na(fms_convergence) && fms_convergence == "tbd"){
  poss_fit_model_safety <-
    purrr::possibly(.f = fit_model_safety_pi, otherwise = "Error")
  single_model_output_fms = poss_fit_model_safety(
    visible_data = visible_data,
    formula = formula,
    formula2 = formula2,
    fm_check = censor_fm_check,
    max_it = max_it,
    ncomp = ncomp,
    tol_ll = tol_ll,
    pi_link = pi_link,
    verbose = verbose,
    maxiter_survreg = maxiter_survreg,
  )
  #   }

  if (length(single_model_output_fms) == 1 &&
      single_model_output_fms == "Error") {
    fms_convergence = FALSE
    if (verbose > 1) {
      print("fit_model_safety failed to converge")
    }
  } else{
    fms_convergence = TRUE
    if (verbose > 1) {
      print("fit_model_safety converged")
    }
  }
}
run_status_notes <-
  c(
    fm_convergence = fm_convergence,
    sigma_check = sigma_check,
    censor_fm_check = censor_fm_check,
    fms_convergence = fms_convergence
  )

output <- list(
  single_model_output = list(single_model_output_fm = single_model_output_fm, single_model_output_fms = single_model_output_fms),
  run_status_notes = run_status_notes
)


}
output_2 = get_2_comp_model(visible_data, formula, formula2, max_it, tol_ll, pi_link, verbose, maxiter_survreg, initial_weighting)


##add checks for 2

##fix 3 then add 3 plus checks and safety mode









}
