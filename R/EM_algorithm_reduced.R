#' Title
#'
#' @inheritParams fit_EM
#' @inheritParams EM_algorithm
#'
#' @param fixed_side
#' @param extra_row
#' @param ecoff
#' @param visible_data
#' @param model
#' @param mu_formula
#' @param pi_formula
#' @param max_it
#' @param ncomp
#' @param tol_ll
#' @param browse_at_end
#' @param browse_each_step
#' @param plot_visuals
#' @param prior_step_plot
#' @param pause_on_likelihood_drop
#' @param pi_link
#' @param verbose
#' @param model_coefficient_tolerance
#' @param maxiter_survreg
#' @param initial_weighting For the reduced model fitting: 1 sets initial weights corresponding to fixed side (and extra_row) where observations not outside the range on the side corresponding to the fixed side are forced to be in the component where mu is being estimated. For initial weighting two a linear model is fit for the component still being estimated to provide initial observation weights.
#' @param sd_initial
#' @param stop_on_likelihood_drop
#' @param non_linear_term
#' @param covariates String, covariates to be included in mu model aside from the non-linear term.
#'
#' @importFrom readr parse_number
#'
#' @return
#' @export
#'
#' @examples
EM_algorithm_reduced = function(
    fixed_side = "RC",
    extra_row = FALSE,
    ecoff = NA,
    visible_data,
    model = "surv", #"mgcv", "polynomial"
    mu_formula = Surv(time = left_bound,
                      time2 = right_bound,
                      type = "interval2") ~ pspline(t, df = 0, caic = TRUE),
    #mu_formula = yi ~ s(t),
    pi_formula = c == "2" ~ s(t), #or: c == "2" ~ lo(t)
    max_it = 3000,
    ncomp = 2,
    tol_ll = 1e-6,
    browse_at_end = FALSE,
    browse_each_step = FALSE,
    plot_visuals = FALSE,
    prior_step_plot = FALSE,
    pause_on_likelihood_drop = FALSE,
    pi_link = "logit",
    verbose = 3,
    model_coefficient_tolerance = 0.00001,
    maxiter_survreg = 30,
    initial_weighting = 1,
    sd_initial = 0.2,
    stop_on_likelihood_drop = FALSE,
    non_linear_term = "t",
    covariates = NULL,
    scale = NULL
){

  if(model == "pspline"){
    model = "surv"
  }


  #add attribute model to visible data

  visible_data = modify_visible_data(visible_data, model)

  visible_data = set_scale_log(visible_data, scale)

  ECOFF = ecoff_into_log(ecoff = ecoff, visible_data = visible_data)

  converge = NA_character_

  if(ncomp == 1){
    errorCondition("The reduced model is appropriate for more than 1 component")
  }else{

    #first E step-----
    possible_data = first_E_step_reduced(initial_weighting = initial_weighting, visible_data = visible_data, plot_visuals = plot_visuals, sd_initial = sd_initial, ncomp = ncomp, non_linear_term = non_linear_term, covariates = covariates, pi_formula = pi_formula, max_it = max_it, tol_ll = tol_ll, pi_link = pi_link, model_coefficient_tolerance = model_coefficient_tolerance, fixed_side = fixed_side, extra_row = extra_row, ECOFF = ECOFF, model = model, verbose = verbose)

    #wrapper function
    #plot_initial_weighting_regression(possible_data = possible_data)
    #browser?
    likelihood_documentation = set_up_likelihood_matrix(max_it)
    possible_data = modify_bounds(possible_data)

    possible_data = add_scale(possible_data, attr(visible_data, "scale"))


      for(i in 1:max_it){
      message_iteration_count(i, verbose)
      #first M step--------
      #MLE of all parameters

      if(i != 1){
        prior_iteration = save_previous_iteration(mu_models_new, pi_model_new, log_likelihood_new, possible_data)
      }

      mu_models_new = fit_all_mu_models(possible_data = possible_data, ncomp = ncomp, mu_formula = mu_formula, approach = "reduced", fixed_side = fixed_side, maxiter_survreg = maxiter_survreg)
      #mu_models_new = fit_all_mu_models(possible_data, ncomp, mu_formula, maxiter_survreg)
      #likelihood_documentation = M_step_likelihood_matrix_updates(likelihood_documentation, i, mu_models_new, ncomp, maxiter_survreg) ###use dimnames in matrix
      if(check_mu_models_convergence(mu_models_new, ncomp - 1)){
        converge = "NO"
        break #if wrapping into an M-step function, add list to list of outputs, then check between steps
      }

      pi_model_new = fit_mgcv_pi_model(pi_formula = pi_formula, pi_link = pi_link, possible_data = possible_data)

      if(i > 1){
        model_coefficient_checks_results = model_coefficient_checks(mu_models_new, pi_model_new, prior_iteration$mu_models_old, prior_iteration$pi_model_old, model_coefficient_tolerance, ncomp-1)
      }




      #Next E step-------------

      # likelihood_documentation[i, "m_step_check_new"] <- m_step_check_maximizing(possible_data, mu_models_new, pi_model_new) ###use dimnames in matrix
      # if(i > 1){
      #   likelihood_documentation[i, "m_step_check_old"] <- m_step_check_maximizing(possible_data, prior_iteration$mu_models_old, prior_iteration$pi_model_old) ###use dimnames in matrix
      # }else{
      #   likelihood_documentation[i, "m_step_check_old"] <- NaN
      # }

      possible_data = E_step_reduced(possible_data, mu_models_new, pi_model_new, fixed_side = fixed_side, extra_row = extra_row, ECOFF = ECOFF)

      if(verbose > 2){
        print(pi_model_new)
        print(mu_models_new)
      }



      log_likelihood_new = calculate_log_likelihood(possible_data, verbose)
      likelihood_documentation[i, "loglikelihood"] <- log_likelihood_new ###use dimnames

      if(i > 1 && likelihood_documentation[i, "loglikelihood"] < likelihood_documentation[i - 1, "loglikelihood"] & stop_on_likelihood_drop){
        converge = "likelihood decreased"
        break
      }

      if(i > 1 & pause_on_likelihood_drop && likelihood_documentation[i, "loglikelihood"] < likelihood_documentation[i - 1, "loglikelihood"]){
        browser("likelihood decreased")
      }

      # if(browse_each_step & plot_visuals){
      #
      #   browser(message("End of step ", i))
      #   plot_fm_step(pi_model_new, mu_models_new, ncomp, possible_data, prior_step_plot, i)
      #
      #   if(i > 1){
      #     plot_likelihood(likelihood_documentation, format = "matrix")
      #   }
      # }

      if(browse_each_step & !plot_visuals){browser(message("End of step ", i))}

      if(i != 1){
        ##too much overlap with 103
        check_ll = (log_likelihood_new - prior_iteration$log_likelihood_old)

        if(check_ll < 0 & verbose >= 2){
          warning("Log Likelihood decreased")  ###  HAS BEEN GETTING USED A LOT, WHY IS THE LOG LIKELIHOOD GOING DOWN????
        }



        if(check_ll < tol_ll & model_coefficient_checks_results)
        {
          if(verbose > 0){
            message("Stopped on combined LL and parameters")}
          converge = "YES"
          break
        }

      }



    }


    ####group 149 to 173
    if(i > 1){
      if(i == max_it & !((check_ll < tol_ll) & model_coefficient_checks_results)){
        converge = "iterations"
      }
    }

    if(i == 1 & i == max_it & is.na(converge)){
      converge = "iterations"
    }


    if(i == 1){
      prior_iteration = list(
        mu_models_old = NA,
        pi_model_old = NA,
        log_likelihood_old = NA,
        possible_data_old = NA
      )
    }

    return(
      list(
        likelihood = tibble_like(likelihood_documentation),
        model = model,
        possible_data = possible_data,
        pi_model = pi_model_new,
        mu_model = mu_models_new,
        steps = i,
        converge = converge,
        ncomp = ncomp,
        prior_step_models = prior_iteration,
        sd_initial = sd_initial,  # ifelse(initial_weighting >= 7, sd_initial, NaN),
        mu_formula = mu_formula,
        fixed_side = fixed_side,
        extra_row = extra_row,
        ecoff = ecoff,
        ECOFF =  ECOFF
      )
    )
  }
}


ecoff_into_log = function(ecoff, visible_data){  if(!is.na(ecoff)){
  if(is.character(ecoff)){
    ECOFF = readr::parse_number(as.character(ecoff)) %>% log2()
  }else{
    ECOFF = log2(ecoff)
  }
  if((ECOFF > max(visible_data$left_bound)) | (ECOFF < min(visible_data$right_bound))){
    errorCondition("ecoff is outside the range of the data, will not split anything")
  }

}else{
  ECOFF = NA
}
  return(ECOFF)
}

