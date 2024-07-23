##for fitting mu models for standard and reduced models, both mgcv and surv/polynomial
fit_all_mu_models_v2 = function(possible_data, ncomp, mu_formula, fixed_side = NULL, maxiter_survreg = 30){
  fitted_comp = case_when(
    is.null(fixed_side) ~ c(1:ncomp),
    !is.null(fixed_side) && fixed_side == "LC" ~ c(2),
    !is.null(fixed_side) && fixed_side == "RC" ~ c(1),
    TRUE ~ -1
  ) %>% unique()
if(length(fitted_comp) == 1 && fitted_comp < 0){
  errorCondition("Invalid value of fixed side, use 'LC', 'RC', or 'NULL'")
}
  if(!is.list(mu_formula)){
    mu_formula = list(mu_formula)
  }
  if(length(mu_formula) < length(fitted_comp) && ((length(fitted_comp) %% length(mu_formula)) %% length(mu_formula)) == 0){
    message(length)
    rep(mu_formula, (length(fitted_comp) / length(mu_formula)) )
  }

  if(is.list(mu_formula) && length(mu_formula) == length(fitted_comp)){

    if(attr(possible_data, "model") %in% c("surv", "polynomial")){
      mu_models_new = purrr::map2(1:ncomp, mu_formula, ~fit_mu_model(possible_data = possible_data, pred_comp = .x, mu_formula = .y, maxiter_survreg = maxiter_survreg))
      mu_models_new = purrr::map(mu_models_new, ~set_model_attr(.x, possible_data))
      attr(mu_models_new, "model") <- attr(possible_data, "model")
    }else if(attr(possible_data, "model") == "mgcv"){
      mu_models_new = purrr::map2(1:ncomp, mu_formula, ~fit_mu_model.mgcv(possible_data = possible_data, pred_comp = .x, mu_formula = .y))
      mu_models_new = purrr::map(mu_models_new, ~set_model_attr(.x, possible_data))
      attr(mu_models_new, "model") <- attr(possible_data, "model")
    }else{
      errorCondition("model must be 'surv', 'polynomial', or 'mgcv'")
    }

  }else{
    errorCondition("mu_formula should be a list of formulas equal to the number of component means being estimated or a single formula to be repeated for all the components")
  }
}
