#' Title
#'
#' @param initial_weighting
#' @param visible_data
#' @param plot_visuals
#' @param sd_initial
#' @param ncomp
#' @param randomize
#' @param n_models
#' @param model
#' @param non_linear_term
#' @param covariates
#' @param pi_formula
#' @param max_it
#' @param tol_ll
#' @param pi_link
#' @param model_coefficient_tolerance
#'
#' @importFrom tidyr pivot_wider
#'
#' @return
#' @export
#'
#' @examples
first_E_step = function(initial_weighting, visible_data, plot_visuals, sd_initial = 0.2, ncomp = 2, randomize = "all", n_models = 100, model,  non_linear_term, covariates, pi_formula, max_it,tol_ll, pi_link, model_coefficient_tolerance){
  if(initial_weighting == 1){
    possible_data = initial_weighting_staggered_weighting_by_distance_from_median_and_boundary(visible_data)
  } else if(initial_weighting == 2){
    possible_data = initial_weighting_staggered_weighting_by_distance_from_median_and_boundary_plus_random_variation(visible_data)
  }else if(initial_weighting == 3){
    possible_data = initial_weighting_flat_interval_censored_full_weight_left_right_censored(visible_data)
  } else if(initial_weighting == 4){
    possible_data = initial_weighting_slight_shift_at_median(visible_data)
  }else if(initial_weighting == 5){
    possible_data = initial_weighting_flat_center_band_of_heavy_weights_at_ends(visible_data)
  }else if(initial_weighting == 6){
    possible_data = initial_weighting_flat_center_two_bands_of_progressively_heavier_weights_at_ends(visible_data)
  } else if(initial_weighting == 7){
    possible_data = random_start(visible_data, ncomp, sd_parameter = sd_initial, n_models, randomize)
  }else if(initial_weighting == 8){
    possible_data = initial_weighting_fixed_regression_at_boundaries(visible_data, ncomp, sd_parameter = sd_initial)
  }else{
    linear_e_step_output = initial_weighting_fit_linear_model(visible_data, non_linear_term, covariates, pi_formula, max_it, ncomp, tol_ll, pi_link, verbose = 0, model_coefficient_tolerance, sd_initial)
    possible_data = linear_e_step_output$possible_data
  }
  attr(possible_data, "plot_initial") <- (plot_visuals & initial_weighting == 8)
  possible_data = add_attribute_data(possible_data, model)
  possible_data = modify_bounds(possible_data)
  return(possible_data)
}
