#' Title
#'
#' @param args
#' @param batch_size
#' @param run_name
#' @param n
#' @param t_dist
#' @param pi
#' @param `E[X|T,C]`
#' @param sd_vector
#' @param covariate_list
#' @param covariate_effect_vector
#' @param covariate_names
#' @param low_con
#' @param high_con
#' @param scale
#' @param formula
#' @param max_it
#' @param ncomp
#' @param tol_ll
#' @param verbose
#'
#' @return
#' @export
#'
#' @importFrom dplyr tibble mutate inner_join group_by case_when
#' @importFrom magrittr %>%
#' @importFrom purrr map as_vector map_chr possibly quietly
#' @importFrom gridExtra grid.arrange
#'
#' @examples
modded_local_full_run_function <- function(args,
                                           batch_size = 10,
                                           run_name,
                                           n,
                                           t_dist,
                                           pi,
                                           `E[X|T,C]`,
                                           sd_vector,
                                           covariate_list = NULL,
                                           covariate_effect_vector = c(0),
                                           covariate_names = NULL,
                                           low_con,
                                           high_con,
                                           scale = "log",
                                           formula,
                                           max_it = 3000,
                                           ncomp = 2,
                                           tol_ll = 1e-6,
                                           verbose = 3){
  iteration_set <- ((batch_size * args) - (batch_size - 1)):(batch_size * args)

  poss_full_sim_in_1_function <- purrr::possibly(.f = full_sim_in_1_function, otherwise = "Error")
  #modded_poss_full_sim_in_1_function <- purrr::quietly(full_sim_in_1_function)
  modded_poss_full_sim_in_1_function <- purrr::quietly(poss_full_sim_in_1_function)

  #   attr_modded_poss_full_sim_in_1_function <- function(i, n, t_dist, pi, `E[X|T,C]`, sd_vector, covariate_list, covariate_effect_vector, covariate_names, low_con, high_con, scale, formula, max_it, ncomp, tol_ll, verbose){
  #       full_results <- modded_poss_full_sim_in_1_function(
  #         i = i,
  #         n = n,
  #         t_dist = t_dist,
  #         pi = pi,
  #         `E[X|T,C]` = `E[X|T,C]`,
  #         sd_vector = sd_vector,
  #         covariate_list = covariate_list,
  #         covariate_effect_vector = covariate_effect_vector,
  #         covariate_names = NULL,
  #         low_con = low_con,
  #         high_con = high_con,
  #         scale = scale,
  #         formula = formula,
  #         max_it = max_it,
  #         ncomp = ncomp,
  #         tol_ll = tol_ll,
  #         verbose = verbose
  #       )
  #
  #       results <- full_results$result
  #       attr(results, "survreg_failure") <- any(stringr::str_detect(full_results$warnings, "Ran out of iterations and did not converge"))
  #       return(results)
  # }

  #run--------
  # results <- purrr::map(
  #   iteration_set,
  #   ~ attr_modded_poss_full_sim_in_1_function(
  #     .x,
  #     n = n,
  #     t_dist = t_dist,
  #     pi = pi,
  #     `E[X|T,C]` = `E[X|T,C]`,
  #     sd_vector = sd_vector,
  #     covariate_list = covariate_list,
  #     covariate_effect_vector = covariate_effect_vector,
  #     covariate_names = NULL,
  #     low_con = low_con,
  #     high_con = high_con,
  #     scale = scale,
  #     formula = formula,
  #     max_it = max_it,
  #     ncomp = ncomp,
  #     tol_ll = tol_ll,
  #     verbose = 3
  #   ))
  #
  full_results <- purrr::map(
    iteration_set,
    ~ modded_poss_full_sim_in_1_function(
      .x,
      n = n,
      t_dist = t_dist,
      pi = pi,
      `E[X|T,C]` = `E[X|T,C]`,
      sd_vector = sd_vector,
      covariate_list = covariate_list,
      covariate_effect_vector = covariate_effect_vector,
      covariate_names = NULL,
      low_con = low_con,
      high_con = high_con,
      scale = scale,
      formula = formula,
      max_it = max_it,
      ncomp = ncomp,
      tol_ll = tol_ll,
      verbose = 3
    ))

  #add_failure_attr <- function(full_results){
  #  results <- full_results$result
  #  attr(results, "survreg_failure") <- any(stringr::str_detect(full_results$warnings, "Ran out of iterations and did not converge"))
  #  results
  #}

  results <- purrr::map(full_results, ~add_failure_attr(.x))

  ##add a save here
  #results <- full_results[[1]]$result
  #attr(results, "survreg_failure") <- any(stringr::str_detect(full_results[[1]]$warnings, "Ran out of iterations and did not converge"))
  #if placed here, they just grab one object from the purrr list output, so it doesn't do it for each object

  file_name <- paste(run_name, args, sep = "_")
  path <- paste0(file_name, ".Rdata")

  save(results, file = path)
}
