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
                                   t_dist = function(n){runif(n, min = 0, max = 10)},
                                   #pi = function(t) {z <- 0.6 #0.5 + 0.2 * t
                                   pi = function(t) {m <- 0.6 + 0.02 * (t ^ 2) - 0.0015 * (t ^ 3)   #logit
                                   z <- exp(m) / (1+ exp(m))
                                   c("1" = 1 - z, "2" = z)},
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
  if (verbose > 2) {
    message("starting run number", i)
  }

  settings = list(
    i,
    n = 150,
    t_dist = function(n) {
      runif(n, min = 0, max = 10)
    },
    #pi = function(t) {z <- 0.6 #0.5 + 0.2 * t
    pi = function(t) {
      m <- 0.6 + 0.02 * (t ^ 2) - 0.0015 * (t ^ 3)   #logit
      z <- exp(m) / (1 + exp(m))
      c("1" = 1 - z, "2" = z)
    },
    #pi = function(t) {z <- 0.6 + 0.03 * t  ##identity
    #c("1" = z, "2" = 1 - z)},
    `E[X|T,C]` = function(t, c)
    {
      case_when(c == "1" ~ -2 - 0.01 * (t ^ 2), #3 + t + 2*t^2 - sqrt(t),
                c == "2" ~ 2 + 0.2 * t,
                TRUE ~ NaN)
    },
    sd_vector = c("1" = 1, "2" = 1),
    covariate_list = NULL,
    covariate_effect_vector = c(0),
    covariate_names = NULL,
    conc_limits_table = NULL,
    #conc_limits_table = as_tibble(rbind(c("a", 2^-3, 2^3),
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
    formula2 = c == "2" ~ s(t),
    ###if pi is defined as a function where we look at change in membership of group 1, then so should formula2
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
    keep_true_values = TRUE
  )
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
  if (keep_true_values) {
    visible_data <-
      prep_sim_data_for_em(
        data.sim,
        left_bound_name = "left_bound",
        right_bound_name = "right_bound",
        time = "t",
        covariate_names = covariate_names,
        scale = scale,
        keep_truth = FALSE,
        observed_value_name = "observed_value",
        comp_name = "comp"
      )
  } else{
    visible_data <-
      prep_sim_data_for_em(
        data.sim,
        left_bound_name = "left_bound",
        right_bound_name = "right_bound",
        time = "t",
        covariate_names = covariate_names,
        scale = scale
      )
  }
  #if(!pi_function){
  #  #mem here
  #  poss_fit_model <- purrr::possibly(.f = fit_model, otherwise = "Error")
  #  single_model_output = poss_fit_model(visible_data = visible_data, formula = formula, max_it = max_it, ncomp = ncomp, tol_ll = tol_ll, verbose = verbose, maxiter_survreg = maxiter_survreg, initial_weighting = initial_weighting)
  #  #single_model_output = fit_model(visible_data = visible_data, formula = formula, max_it = max_it, ncomp = ncomp, tol_ll = tol_ll, verbose = verbose, maxiter_survreg = maxiter_survreg, ...)
  #} else{
  poss_fit_model <-
    purrr::possibly(.f = fit_model_pi, otherwise = "Error")
  single_model_output = poss_fit_model(
    visible_data = visible_data,
    formula = formula,
    formula2 = formula2,
    max_it = max_it,
    ncomp = ncomp,
    tol_ll = tol_ll,
    pi_link = pi_link,
    verbose = verbose,
    maxiter_survreg = maxiter_survreg,
    initial_weighting = initial_weighting
  )

  #}


  if (length(single_model_output) > 1) {
    fm_check <-
      check_for_excessive_censoring(single_model_output, cutoff)

  }


  if (fms_only == TRUE &&
      length(single_model_output) > 1 && fm_check == "All Clear") {
    single_model_output <- "Pass"
  }




  #wrap fit model with possibly() or try()

  if (length(single_model_output) == 1 &&
      single_model_output == "Error") {
    fm_fail = "fm_failed"
  } else if (fm_check == "Excessive Censoring") {
    fm_fail = "fm_failed_cutoff"
  } else{
    fm_fail = "fm_worked"
  }

  if (fm_fail %in% c("fm_failed, fm_failed_cutoff") &
      allow_safety == TRUE) {
    formula = Surv(time = left_bound,
                   time2 = right_bound,
                   type = "interval2") ~ pspline(t, df = 0, calc = TRUE)  ####FIX FOR MORE COMPLEX MODELS
    #    if(!pi_function){
    #    poss_fit_model_safety <- purrr::possibly(.f = fit_model_safety, otherwise = "Error")
    #    #single_model_output = fit_model_safety(visible_data = visible_data, formula = formula, max_it = max_it, ncomp = ncomp, tol_ll = tol_ll, verbose = verbose, maxiter_survreg = maxiter_survreg, ...)
    #    single_model_output = poss_fit_model_safety(visible_data = visible_data, formula = formula, max_it = max_it, ncomp = ncomp, tol_ll = tol_ll, verbose = verbose, maxiter_survreg = maxiter_survreg)
    #    } else{
    poss_fit_model_safety <-
      purrr::possibly(.f = fit_model_safety_pi, otherwise = "Error")
    single_model_output = poss_fit_model_safety(
      visible_data = visible_data,
      formula = formula,
      formula2 = formula2,
      max_it = max_it,
      ncomp = ncomp,
      tol_ll = tol_ll,
      pi_link = pi_link,
      verbose = verbose,
      maxiter_survreg = maxiter_survreg
    )
    #   }

    if (length(single_model_output) == 1 &&
        single_model_output == "Error") {
      fms_fail = "fms_failed"
    } else{
      fms_fail = "fms_worked"
    }


  } else{
    fms_fail = "fms_not_called"
  }

  #evaluate
  #run fit_model_safely if needed

  failure_safety_notes <-
    c(fms_only, allow_safety, fm_fail, fms_fail)

  output <- list(single_model_output, settings, failure_safety_notes)



  return(output)

}
