#' local_full_run_function
#'
#' @param args
#' @param n
#' @param t_dist
#' @param pi
#' @param `E[X|T,C]`
#' @param sd_vector
#' @param covariate_list
#' @param covariate_effect_vector
#' @param low_con
#' @param high_con
#' @param scale
#' @param formula
#' @param max_it
#' @param ncomp
#' @param tol_ll
#' @param maxiter_survreg
#' @param verbose
#' @param allow_safety
#' @param cutoff
#'
#' @importFrom dplyr tibble mutate inner_join group_by case_when
#' @importFrom magrittr %>%
#' @importFrom purrr map as_vector map_chr possibly
#' @importFrom gridExtra grid.arrange
#'
#' @return
#' @export
#'
#' @examples
local_full_run_function <- function(args,
                                    batch_size = 10,
                                    run_name,
                                    rerun_parameters){
  iteration_set <- ((batch_size * args) - (batch_size - 1)):(batch_size * args)

  rerun_parameters$iteration_set <- iteration_set

  #run--------
  model_results <- purrr::map(
    iteration_set,
    ~ full_sim_in_1_function(
      .x,
      n = rerun_parameters$n,
      t_dist = rerun_parameters$t_dist,
      pi = rerun_parameters$pi,
      `E[X|T,C]` = rerun_parameters$`E[X|T,C]`,
      sd_vector = rerun_parameters$sd_vector,
      covariate_list = rerun_parameters$covariate_list,
      covariate_effect_vector = rerun_parameters$covariate_effect_vector,
      covariate_names = rerun_parameters$covariate_names,
      conc_limits_table = rerun_parameters$conc_limits_table,
      low_con = rerun_parameters$low_con,
      high_con = rerun_parameters$high_con,
      scale = rerun_parameters$scale,
      formula = rerun_parameters$formula,
      formula2 = rerun_parameters$formula2,
      max_it = rerun_parameters$max_it,
      ncomp = rerun_parameters$ncomp,
      tol_ll = rerun_parameters$tol_ll,
      maxiter_survreg = rerun_parameters$maxiter_survreg,
#      pi_function = pi_function,
      pi_link = rerun_parameters$pi_link,
      verbose = rerun_parameters$verbose,
      allow_safety = rerun_parameters$allow_safety,
      cutoff = rerun_parameters$cutoff,
      fms_only = rerun_parameters$fms_only,
      initial_weighting = rerun_parameters$initial_weighting,
      keep_true_values = rerun_parameters$keep_true_values
    ))

  results <- list(
    model_results = model_results,
    settings = rerun_parameters
    )



  file_name <- paste(run_name, args, sep = "_")
  path <- paste0(file_name, ".Rdata")

  save(results, file = path)
}
