#' full_sim_in_1_function
#'
#' @param i
#' @param n
#' @param t_dist
#' @param pi
#' @param `E[X|T,C]`
#' @param sd_vector
#' @param covariate_list
#' @param covariate_effect_vector
#' @param covariate_names
#' @param conc_limits_table
#' @param low_con
#' @param high_con
#' @param scale
#' @param formula
#' @param formula2
#' @param max_it
#' @param ncomp
#' @param tol_ll
#' @param maxiter_survreg
#' @param pi_function
#' @param pi_link
#' @param verbose
#' @param allow_safety
#' @param cutoff
#' @param fms_only
#' @param initial_weighting
#' @param keep_true_values
#'
#' @return
#' @export
#'
#' @importFrom dplyr tibble mutate inner_join group_by case_when
#' @importFrom magrittr %>%
#' @importFrom purrr map as_vector map_chr
#' @importFrom gridExtra grid.arrange
#' @importFrom survival survreg survreg.control
#' @importFrom
#'
#' @examples
full_sim_in_1_function <- function(i,
                                   n = 150,
                                   t_dist = function(n){runif(n, min = 0, max = 15)},
                                   #pi = function(t) {z <- 0.6 #0.5 + 0.2 * t
                                   pi = function(t) {m <- 0.6 + 0.02 * (t ^ 2) - 0.0015 * (t ^ 3)   #logit
                                   z <- exp(m) / (1+ exp(m))
                                   tibble("1" = 1 - z, "2" = z)},
                                   #pi = function(t) {z <- 0.6 + 0.03 * t  ##identity
                                   #c("1" = z, "2" = 1 - z)},
                                   `E[X|T,C]` = function(t, c)
                                   {
                                     case_when(
                                       c == "1" ~ -2 - 0.01 * (t ^ 2), #3 + t + 2*t^2 - sqrt(t),
                                       c == "2" ~ 2 + 0.2*t,
                                       TRUE ~ NaN
                                     )
                                   },
                                   sd_vector = c("1" = 1, "2" = 1),
                                   covariate_list = NULL,
                                   covariate_effect_vector = c(0),
                                   covariate_names = NULL,
                                   conc_limits_table = NULL, #conc_limits_table = as_tibble(rbind(c("a", 2^-3, 2^3),
                                   #c("b", 2^-4, 2^4)), `.name_repair` = "unique"
                                   #) %>% rename("covariate_2" = 1, "low_cons" = 2, "high_cons" = 3),
                                   low_con = -4,
                                   high_con = 4,
                                   scale = "log",
                                   #formula = Surv(time = left_bound,
                                   #               time2 = right_bound,
                                   #               type = "interval2") ~ 0 + c + strata(c) + t:c,
                                   #for split use this:
                                   formula = Surv(time = left_bound,
                                                        time2 = right_bound,
                                                        type = "interval2") ~ pspline(t, df = 0, calc = TRUE),
                                   formula2 = c == "2" ~ s(t),  ###if pi is defined as a function where we look at change in membership of group 1, then so should formula2
                                   max_it = 3000,
                                   ncomp = 2,
                                   tol_ll = 1e-6,
                                   #silent = FALSE,
                                   maxiter_survreg = 30,
                                   #pi_function = TRUE,
                                   pi_link = "logit",
                                   verbose = 3,
                                   allow_safety = TRUE,
                                   cutoff = 0.9,
                                   fms_only = FALSE,
                                   initial_weighting = 1,
                                   keep_true_values = TRUE,
                                   ...
){
  set.seed(i)
  if (verbose >= 2) {
    message("starting run number", i)
  }

  if (fms_only == TRUE & allow_safety == FALSE) {
    errorCondition("Invalid combination of fms_only and allow_safety, cannot have fms_only == TRUE and allow_safety = FALSE")
  }

  ##need i = i, n = n, t_dist = t_dist to create named list

  # settings = list( #name all settings, at batch level: create settings and save along with results
  #   i,
  #   n,
  #   t_dist,
  #   pi,
  #   `E[X|T,C]`,
  #   sd_vector,
  #   covariate_list,
  #   covariate_effect_vector ,
  #   covariate_names,
  #   conc_limits_table,
  #   low_con,
  #   high_con,
  #   scale,
  #   formula,
  #   formula2,
  #   max_it,
  #   ncomp,
  #   tol_ll,
  #   maxiter_survreg,
  #   pi_link,
  #   verbose,
  #   allow_safety,
  #   cutoff,
  #   fms_only,
  #   initial_weighting,
  #   keep_true_values
  # )
  #verbose = 0: print nothing
  #verbose = 1: print run number
  #verbose = 2: print run number and iteration number
  #verbose = 3: print run number, iteration number, and iteration results
  #verbose = 4: print run number, iteration number, iteration results, and run aft as verbose


  #mem here

  data.sim <- simulate_mics(
    #changed to test
    n = n,
    t_dist = t_dist,
    pi = pi,
    `E[X|T,C]` = `E[X|T,C]`,
    sd_vector = sd_vector,
    covariate_list = covariate_list,
    covariate_effect_vector = covariate_effect_vector,
    conc_limits_table = conc_limits_table,
    low_con = low_con,
    high_con = high_con,
    scale = scale
  )

  #mem here

  #visible_data <- data.sim %>% mutate(obs_id = 1:n()) %>%
  #  relocate(obs_id, .before = everything())

  visible_data <-
    prep_sim_data_for_em(
      data.sim,
      left_bound_name = "left_bound",
      right_bound_name = "right_bound",
      time = "t",
      covariate_names = covariate_names,
      scale = scale,
      keep_truth = keep_true_values,
      observed_value_name = "observed_value",
      comp_name = "comp"
    )

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

  if (prelim_cens_check %>% filter(cens != "interval_censored") %>% pull(proportion) %>% sum >= 0.8) {
    overall_censoring = "stop"
    fm_convergence = NA
    sigma_check = NA_character_
    censor_fm_check = NA_character_
    fms_convergence = NA
    single_model_output_fm = "PASS"
  } else{
    overall_censoring = "go"


    #if(!pi_function){
    #  #mem here
    #  poss_fit_model <- purrr::possibly(.f = fit_model, otherwise = "Error")
    #  single_model_output = poss_fit_model(visible_data = visible_data, formula = formula, max_it = max_it, ncomp = ncomp, tol_ll = tol_ll, verbose = verbose, maxiter_survreg = maxiter_survreg, initial_weighting = initial_weighting)
    #  #single_model_output = fit_model(visible_data = visible_data, formula = formula, max_it = max_it, ncomp = ncomp, tol_ll = tol_ll, verbose = verbose, maxiter_survreg = maxiter_survreg, ...)
    #} else{
    poss_fit_model <-
      purrr::possibly(.f = fit_model_pi, otherwise = "Error")
    single_model_output_fm = poss_fit_model(
      visible_data = visible_data,
      formula = formula,
      formula2 = formula2,
      max_it = max_it,
      ncomp = ncomp,
      tol_ll = tol_ll,
      pi_link = pi_link,
      verbose = verbose,
      maxiter_survreg = maxiter_survreg,
      initial_weighting = initial_weighting,
    )

    #}

    if (length(single_model_output_fm) == 1) {
      fm_convergence = FALSE
      sigma_check = NA_character_
      censor_fm_check  = NA_character_
      fms_convergence = NA ##Check on why the fm runs are erroring out, can't send it to fms without a side
      if (verbose > 1) {
        print("fit_model failed to converge")
      }
    }
    else {
      fm_convergence = TRUE

      if (verbose > 1) {
        print("fit_model converged")
      }
    }

    if(fm_convergence){
      max_scale <-
        max(single_model_output_fm$newmodel[[1]]$scale,
            single_model_output_fm$newmodel[[2]]$scale)
      max_difference_mu_hat <-
        max(abs(
          predict(single_model_output_fm$single_model_output_fm$newmodel[[2]], newdata = visible_data) - predict(single_model_output_fm$single_model_output_fm$newmodel[[1]], newdata = visible_data)
        ))
      if (max_scale ^ 2 > max_difference_mu_hat) {
        sigma_check = "stop"
        #censor_fm_check = NA_character_
        #fms_convergence = "tbd"
      } else{

        sigma_check = "go"
}

      censor_fm_check <-
          check_for_excessive_censoring(single_model_output_fm, cutoff)

        if (censor_fm_check == "BOTH") {
          fms_convergence = NA
          single_model_output_fms = "PASS"
          if (verbose > 1) {
            print(
              "fit_model converged outside excessive censoring boundaries in both directions"
            )
          }
        } else if (censor_fm_check == "ALl Clear" & sigma_check == "go") {
          fms_convergence = NA
          single_model_output_fms = "PASS"
          if (verbose > 1) {
            print("fit_model converged within excessive censoring boundaries")
          }
        } else if (censor_fm_check == "ALl Clear" & sigma_check == "stop") {
          fms_convergence = NA
          single_model_output_fms = "PASS"
          if (verbose > 1) {
            print("fit_model converged within excessive censoring boundaries but violated sigma check, cannot find a side to use for fms")
          }
        } else if (censor_fm_check %in% c("RC", "LC") &
                   allow_safety == FALSE) {
          if (verbose > 1) {
            print("fit_model converged but violated excessive censoring cutoff")
          }
          single_model_output_fms = "PASS"
          fms_convergence = NA
        } else{
          fms_convergence = "tbd"
        }
      }
      }

    if(!is.na(fms_convergence)){


          if (verbose > 1) {
            print(paste0("running fit_model_safety ", censor_fm_check))
          }
          # formula = Surv(time = left_bound,
          #                time2 = right_bound,
          #                type = "interval2") ~ pspline(t, df = 0, calc = TRUE)  ####I think formula can remain unchanged?


          #    if(!pi_function){
          #    poss_fit_model_safety <- purrr::possibly(.f = fit_model_safety, otherwise = "Error")
          #    #single_model_output = fit_model_safety(visible_data = visible_data, formula = formula, max_it = max_it, ncomp = ncomp, tol_ll = tol_ll, verbose = verbose, maxiter_survreg = maxiter_survreg, ...)
          #    single_model_output = poss_fit_model_safety(visible_data = visible_data, formula = formula, max_it = max_it, ncomp = ncomp, tol_ll = tol_ll, verbose = verbose, maxiter_survreg = maxiter_survreg)
          #    } else{
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

    } else{
  single_model_output_fms = "PASS"}







  #evaluate
  #run fit_model_safely if needed

  run_status_notes <-
    c(
      overall_censoring = overall_censoring,
      fm_convergence = fm_convergence,
      sigma_check = sigma_check,
      censor_fm_check = censor_fm_check,
      fms_convergence = fms_convergence
    )

  if(fms_only){
    single_model_output_fm = "PASS"
  }

  output <- list(
    single_model_output = list(single_model_output_fm = single_model_output_fm, single_model_output_fms = single_model_output_fms),
    i = i,
    run_status_notes = run_status_notes
  )



  return(output)


}
