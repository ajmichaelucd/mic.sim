#' Title
#'
#' @param possible_data
#' @param ncomp
#' @param mu_formula
#' @param fixed_side
#' @param maxiter_survreg
#'
#' @return
#' @keywords internal
#'
#' @examples
fit_all_mu_models = function(possible_data, ncomp, mu_formula, approach = NULL, fixed_side = NULL, maxiter_survreg = 30){
  fitted_comp = case_when(
    !is.null(approach) && approach == "full" ~ c(1:ncomp),
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
    message("length of mu_formula is shorter than number of components, recycling mu_formula for all components")
    mu_formula = rep(mu_formula, (length(fitted_comp) / length(mu_formula)) )
  }

  if(is.list(mu_formula) && length(mu_formula) == length(fitted_comp)){

    if(attr(possible_data, "model") %in% c("surv", "polynomial")){
      mu_models_new = purrr::map2(fitted_comp, mu_formula, ~fit_mu_model_safe_formatted(possible_data = possible_data, pred_comp = .x, mu_formula = .y, maxiter_survreg = maxiter_survreg))
      mu_models_new = purrr::map(mu_models_new, ~set_model_attr(.x, possible_data))
      attr(mu_models_new, "model") <- attr(possible_data, "model")
    }else if(attr(possible_data, "model") == "mgcv"){
      mu_models_new = purrr::map2(fitted_comp, mu_formula, ~fit_mu_model.mgcv(possible_data = possible_data, pred_comp = .x, mu_formula = .y))
      mu_models_new = purrr::map(mu_models_new, ~set_model_attr(.x, possible_data))
      attr(mu_models_new, "model") <- attr(possible_data, "model")
    }else{
      errorCondition("model must be 'surv', 'polynomial', or 'mgcv'")
    }

  }else{
    errorCondition("mu_formula should be a list of formulas equal to the number of component means being estimated or a single formula to be repeated for all the components")
  }

  attr(mu_models_new, "fixed_side") <- fixed_side
  return(mu_models_new)
}


set_model_attr = function(model, possible_data){attr(model, "model")  = attr(possible_data, "model")
return(model)}

fit_all_mu_models.mgcv = function(possible_data, ncomp, mu_formula){
  mu_models_new = purrr::map(1:ncomp, ~fit_mu_model.mgcv(possible_data = possible_data, pred_comp = .x, mu_formula = mu_formula))
  mu_models_new = purrr::map(mu_models_new, ~set_model_attr(.x, possible_data))
  attr(mu_models_new, "model") <- attr(possible_data, "model")
  return(mu_models_new)
}

fit_all_mu_models.polynomial = function(possible_data, ncomp, mu_formula, maxiter_survreg = 30){
  mu_models_new = purrr::map2(1:ncomp, mu_formula, ~fit_mu_model(possible_data = possible_data, pred_comp = .x, mu_formula = .y, maxiter_survreg = maxiter_survreg))
  mu_models_new = purrr::map(mu_models_new, ~set_model_attr(.x, possible_data))
  attr(mu_models_new, "model") <- attr(possible_data, "model")
  return(mu_models_new)
}

fit_all_mu_models.surv.split = function(possible_data, ncomp, mu_formula, maxiter_survreg){
  mu_models_new = purrr::map2(1:ncomp, mu_formula, ~fit_mu_model(possible_data = possible_data, pred_comp = .x, mu_formula = .y, maxiter_survreg = maxiter_survreg))
  mu_models_new = purrr::map(mu_models_new, ~set_model_attr(.x, possible_data))
  attr(mu_models_new, "model") <- attr(possible_data, "model")
  return(mu_models_new)
} ##note is identical to fit_all_mu_models.polynomial

# fit_all_mu_models = function(possible_data, ncomp, mu_formula, maxiter_survreg = 30){
#   if(attr(possible_data, "model") == "mgcv"){
#     fit_all_mu_models.mgcv(possible_data, ncomp, mu_formula) %>% return()
#   }else if(attr(possible_data, "model") == "polynomial"){
#     fit_all_mu_models.polynomial(possible_data, ncomp, mu_formula, maxiter_survreg) %>% return()
#   }else if(attr(possible_data, "model") == "surv" & is.list(mu_formula) && length(mu_formula) == ncomp){
#     fit_all_mu_models.surv.split(possible_data, ncomp, mu_formula, maxiter_survreg) %>% return()
#   }else {
#     fit_all_mu_models.surv(possible_data, ncomp, mu_formula, maxiter_survreg) %>% return()
#   }
# }
