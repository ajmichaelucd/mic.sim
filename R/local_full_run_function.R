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
                                    maxiter_survreg = 30,
                                    verbose = 3,
                                    allow_safety = TRUE,
                                    cutoff = 0.9){
  iteration_set <- ((batch_size * args) - (batch_size - 1)):(batch_size * args)

  #run--------
  results <- purrr::map(
    iteration_set,
    ~ full_sim_in_1_function(
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
      maxiter_survreg = maxiter_survreg,
      verbose = verbose,
      allow_safety = allow_safety,
      cutoff = cutoff
    ))



  ##add a save here



  file_name <- paste(run_name, args, sep = "_")
  path <- paste0(file_name, ".Rdata")

  save(results, file = path)
}
