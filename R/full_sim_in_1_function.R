#' full_sim_in_1_function
#'
#' @param i
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
#' @importFrom purrr map as_vector map_chr
#' @importFrom gridExtra grid.arrange
#' @importFrom
#' @importFrom
#'
#' @examples
full_sim_in_1_function <- function(i,
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
                                   low_con = 2^-4,
                                   high_con = 2^4,
                                   scale = "log",
                                   formula = Surv(time = left_bound,
                                                  time2 = right_bound,
                                                  type = "interval2") ~ 0 + c + strata(c) + t:c,
                                   max_it = 3000,
                                   ncomp = 2,
                                   tol_ll = 1e-6,
                                   #silent = FALSE,
                                   verbose = 3,
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

  data.sim <- simulate_mics(
    n = n,
    t_dist = t_dist,
    pi = pi,
    `E[X|T,C]` = `E[X|T,C]`,
    sd_vector = sd_vector,
    covariate_list = covariate_list,
    covariate_effect_vector = covariate_effect_vector,
    low_con = low_con,
    high_con = high_con,
    scale = "log")

  #mem here

  visible_data <- prep_sim_data_for_em(data.sim, left_bound_name = "left_bound", right_bound_name = "right_bound", time = "t", covariate_names, scale = scale)

  #mem here

  single_model_output = fit_model(visible_data, formula, max_it, ncomp, tol_ll, verbose = verbose, ...)

  single_model_output <- append(single_model_output, i)

  return(single_model_output)

}

