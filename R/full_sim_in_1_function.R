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
#' @param low_con
#' @param high_con
#' @param scale
#' @param formula
#' @param max_it
#' @param ncomp
#' @param tol_ll
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
                                   n = 100,
                                   t_dist = function(n){runif(n, min = 0, max = 1)},
                                   pi = function(t) {z <- 0.5 + 0.2 * t
                                   c("1" = z, "2" = 1- z)},
                                   `E[X|T,C]` = function(t, c)
                                   {
                                     case_when(
                                       c == "1" ~ 3 + t + 2*t^2 - sqrt(t),
                                       c == "2" ~ 3*t,
                                       TRUE ~ NaN
                                     )
                                   },
                                   sd_vector = c("1" = 1, "2" = 2),
                                   covariate_list,
                                   covariate_effect_vector,
                                   low_con = 2^-4,
                                   high_con = 2^4,
                                   scale = "log",
                                   formula = Surv(time = left_bound,
                                                  time2 = right_bound,
                                                  type = "interval2") ~ 0 + c + strata(c) + t:c,
                                   max_it = 3000,
                                   ncomp = 2,
                                   tol_ll = 1e-6
){
  set.seed(i)



  data.sim <- simulate_mics(
    n = n,
    t_dist = t_dist1,
    pi = pi1,
    `E[X|T,C]` = `E[X|T,C]`,
    sd_vector = sd_vector,
    covariate_list = covariate_list,
    covariate_effect_vector = covariate_effect_vector,
    low_con = low_con,
    high_con = high_con,
    scale = "log")


  visible_data <- prep_sim_data_for_em(data.sim, left_bound_name = "left_bound", right_bound_name = "right_bound", time = "t", covariate_names, scale = scale)

  single_model_output = fit_model(visible_data, formula, max_it, ncomp, tol_ll)

  return(single_model_output)

}

