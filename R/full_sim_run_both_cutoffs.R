full_sim_run_both <- function(i,
                              n = 150,
                              t_dist = function(n){runif(n, min = 0, max = 5)},
                              pi = function(t) {z <- 0.6 #0.5 + 0.2 * t
                              c("1" = z, "2" = 1 - z)},
                              `E[X|T,C]` = function(t, c)
                              {
                                case_when(
                                  c == "1" ~ -2 -0.1*t, #3 + t + 2*t^2 - sqrt(t),
                                  c == "2" ~ 2 + 0.2*t,
                                  TRUE ~ NaN
                                )
                              },
                              sd_vector = c("1" = 1, "2" = 2),
                              covariate_list = NULL,
                              covariate_effect_vector = c(0),
                              covariate_names = NULL,
                              conc_limits_table = NULL, #conc_limits_table = as_tibble(rbind(c("a", 2^-3, 2^3),
                              #c("b", 2^-4, 2^4)), `.name_repair` = "unique"
                              #) %>% rename("covariate_2" = 1, "low_cons" = 2, "high_cons" = 3),
                              low_con = -4,
                              high_con = 4,
                              scale = "log",
                              formula = Surv(time = left_bound,
                                             time2 = right_bound,
                                             type = "interval2") ~ 0 + c + strata(c) + t:c,
                              max_it = 3000,
                              ncomp = 2,
                              tol_ll = 1e-6,
                              #silent = FALSE,
                              maxiter_survreg = 30,
                              verbose = 3,
                              allow_safety = TRUE,
                              cutoff = 0.9,

                              ...
){
  set.seed(i)
  if(verbose > 2){
    message("starting run number", i)}
  #verbose = 0: print nothing
  #verbose = 1: print run number
  #verbose = 2: print run number and iteration number
  #verbose = 3: print run number, iteration number, and iteration results
  #verbose = 4: print run number, iteration number, iteration results, and run aft as verbose



  #mem here

  data.sim <- simulate_mics_2( #changed to test
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
    scale = scale)

  #mem here

  visible_data <- prep_sim_data_for_em(data.sim, left_bound_name = "left_bound", right_bound_name = "right_bound", time = "t", covariate_names, scale = scale)

  #mem here
  poss_fit_model <- purrr::possibly(.f = fit_model, otherwise = "Error")
  single_model_output = poss_fit_model(visible_data = visible_data, formula = formula, max_it = max_it, ncomp = ncomp, tol_ll = tol_ll, verbose = verbose, maxiter_survreg = maxiter_survreg)
  #single_model_output = fit_model(visible_data = visible_data, formula = formula, max_it = max_it, ncomp = ncomp, tol_ll = tol_ll, verbose = verbose, maxiter_survreg = maxiter_survreg, ...)
if(length(single_model_output) > 1){

fm_check <- check_for_excessive_censoring(single_model_output, cutoff)

}







  #wrap fit model with possibly() or try()

  if(length(single_model_output) == 1 && single_model_output == "Error"){
    fm_fail = "fm_failed"
  } else if(fm_check == "Excessive Censoring"){
    fm_fail = "fm_failed_cutoff"
  } else{
    fm_fail = "fm_worked"
  }

  if(fm_fail %in% c("fm_failed, fm_failed_cutoff") & allow_safety == TRUE){
    formula = Surv(time = left_bound,
                   time2 = right_bound,
                   type = "interval2") ~ t
    poss_fit_model_safety <- purrr::possibly(.f = fit_model_safety, otherwise = "Error")
    #single_model_output = fit_model_safety(visible_data = visible_data, formula = formula, max_it = max_it, ncomp = ncomp, tol_ll = tol_ll, verbose = verbose, maxiter_survreg = maxiter_survreg, ...)
    single_model_output = poss_fit_model_safety(visible_data = visible_data, formula = formula, max_it = max_it, ncomp = ncomp, tol_ll = tol_ll, verbose = verbose, maxiter_survreg = maxiter_survreg)
    if(length(single_model_output) == 1 && single_model_output == "Error"){
      fms_fail = "fms_failed"
    } else{
      fms_fail = "fms_worked"
    }


  } else{
    fms_fail = "fms_not_called"
  }

  #evaluate
  #run fit_model_safely if needed

  failure_safety_notes <- c(allow_safety, fm_fail, fms_fail)


  single_model_output <- append(single_model_output, i)
  single_model_output <- append(single_model_output, failure_safety_notes)
  #return

  return(single_model_output)

}
