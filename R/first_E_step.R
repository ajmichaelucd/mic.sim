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
#'
#' @return
#' @export
#'
#' @examples
first_E_step = function(initial_weighting, visible_data, plot_visuals, sd_initial = 0.2, ncomp = 2, randomize = "all", n_models = 100, model){
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
  }else{

    possible_data = initial_weighting_fixed_regression_at_boundaries(visible_data, ncomp, sd_parameter = sd_initial)

    # if(plot_visuals){
    #   plot_initial_weighting_regression(possible_data)
    #
    #   #browser("stopping at inital setup to examine a plot for the basis of the initial weighting")
    #
    # }


  }
  attr(possible_data, "plot_initial") <- (plot_visuals & initial_weighting == 8)
  possible_data = add_attribute_data(possible_data, model)
  possible_data = modify_bounds(possible_data)
  return(possible_data)
}
