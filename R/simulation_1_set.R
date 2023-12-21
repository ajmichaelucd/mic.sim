#' Title
#'
#' @param mic.seed
#' @param random.start.seeds
#' @param n
#' @param t_dist
#' @param pi
#' @param `E[X|T,C]`
#' @param sd_vector
#' @param low_con
#' @param high_con
#' @param scale
#' @param model
#' @param mu_formula
#' @param pi_formula
#' @param max_it
#' @param ncomp
#' @param tol_ll
#' @param pi_link
#' @param verbose
#' @param initial_weighting
#' @param model_coefficient_tolerance
#' @param maxiter_survreg
#' @param sd_initial
#' @param randomize
#' @param censored_side
#' @param extra_row
#'
#' @return
#' @export
#'
#' @examples
simulation_1_set =
  function(mic.seed,
         random_start_seeds,
         n,
         t_dist,
         pi,
         `E[X|T,C]`,
         sd_vector,
         low_con,
         high_con,
         scale = "log",
         model = "fm",
         mu_formula = Surv(time = left_bound,
                           time2 = right_bound,
                           type = "interval2") ~ pspline(t, df = 0, caic = TRUE),
         pi_formula = c == "2" ~ s(t),
         max_it = 500,
         ncomp = 2,
         tol_ll = 1e-5,
         pi_link = "logit",
         verbose = 3,
         initial_weighting = 7,
         model_coefficient_tolerance = 0.0001,
         maxiter_survreg = 30,
         sd_initial = 0.2,
         randomize = "all",
         censored_side = NULL,
         extra_row = NULL){
  #sim data once
  set.seed(mic.seed)
  if (verbose >= 2) {
    message("mic.seed ", mic.seed)
  }

  data.sim <- simulate_mics(
    n = n,
    t_dist = t_dist,
    pi = pi,
    `E[X|T,C]` = `E[X|T,C]`,
    sd_vector = sd_vector,
    covariate_list = NULL,
    covariate_effect_vector = c(0),
    conc_limits_table = NULL,
    low_con = low_con,
    high_con = high_con,
    scale = scale
  )

  visible_data <-
    prep_sim_data_for_em(
      data.sim,
      left_bound_name = "left_bound",
      right_bound_name = "right_bound",
      time = "t",
      covariate_names = NULL,
      scale = scale,
      keep_truth = TRUE,
      observed_value_name = "observed_value",
      comp_name = "comp"
    )


  #grid fit 100 models
  if(model == "fm"){
    output = EM_fm_surv_batch_run(
      random_seeds_vector = random_start_seeds,
      visible_data = visible_data,
      mu_formula = mu_formula,
      pi_formula = pi_formula,
      max_it = max_it,
      ncomp = ncomp,
      tol_ll = tol_ll,
      pi_link = pi_link,
      verbose = verbose,
      initial_weighting = initial_weighting,
      model_coefficient_tolerance = model_coefficient_tolerance,
      maxiter_survreg = maxiter_survreg,
      sd_initial = sd_initial,
      randomize = randomize
    )
  }else if(model == "fms"){
    output = EM_fms_surv_batch_run(
      random_seeds_vector = random_start_seeds,
      visible_data = visible_data,
      mu_formula = mu_formula,
      pi_formula = pi_formula,
      censored_side = censored_side,
      extra_row = extra_row,
      max_it = max_it,
      ncomp = ncomp,
      tol_ll = tol_ll,
      pi_link = pi_link,
      verbose = verbose,
      model_coefficient_tolerance = model_coefficient_tolerance,
      maxiter_survreg = maxiter_survreg,
      sd_initial = sd_initial,
      randomize = randomize
    )
  }else{
    errorCondition("pick 'fm' or 'fms'")
  }

  list(set_output = output, set_mic_seed = mic.seed, set_start_seeds = random_start_seeds) %>% return()
}
