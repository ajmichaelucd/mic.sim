#' single_aft_run_new_data
#'
#' Generates a single data set and fits aft models to it, for each type list in type_list parameter
#'
#' @param covariate_effect_vector
#' @param covariate_list
#' @param covariate_names
#' @param n
#' @param t_dist
#' @param pi
#' @param complist
#' @param sd_vector
#' @param low_con
#' @param high_con
#' @param type_list
#' @param time
#' @param MIC_breakpoint
#' @param iteration
#' @param stderr
#'
#' @return
#' @export
#'
#' @importFrom dplyr mutate all_of select bind_rows tibble inner_join group_by
#' @importFrom magrittr %>%
#' @importFrom survival Surv survreg
#' @importFrom broom tidy
#' @importFrom purrr map as_vector
#' @importFrom tidyr pivot_wider
#'
#'
#' @examples


single_aft_run_new_data <- function(covariate_effect_vector = c(0),
                                    covariate_list = NULL,
                                    covariate_names = NULL,
                                    n = 100,
                                    t_dist = function(n){runif(n, min = 0, max = 1)},
                                    pi = function(t) {z <- 1 + 0* t
                                    c("1" = z)},
                                    complist = list(
                                      "1" = function(t) {0 + 0 * t}),
                                    sd_vector =  c("1" = 1),
                                    low_con = 2^-4,
                                    high_con = 2^4,
                                    type_list = c("loglogistic", "lognormal", "weibull", "gaussian", "exponential", "logistic"),
                                    time = "t",
                                    MIC_breakpoint = 1,
                                    #summary = TRUE, #summary must be TRUE for the part that grabs coefficients to work, if needed could run an if statement later for this
                                    iteration = 1,
                                    stderr = FALSE){
if(stderr == FALSE){
  data.sim <- simulate_mics(
    n,
    t_dist,
    pi,
    complist,
    sd_vector,
    covariate_list,
    covariate_effect_vector,
    low_con,
    high_con)

  purrr::map(
    type_list,
    ~ grab_aft_output_without_se(
      data.sim,
      time,
      covariate_names,
      data.sim$left_bound,
      data.sim$right_bound,
      .x,
      summary = TRUE
    )
  ) %>% dplyr::bind_rows() %>%
    tidyr::pivot_wider(names_from = name, values_from = estimate) %>%
    mutate(iteration = iteration)
}

  else{
    data.sim <- simulate_mics(
      n,
      t_dist,
      pi,
      complist,
      sd_vector,
      covariate_list,
      covariate_effect_vector,
      low_con,
      high_con)

    purrr::map(
      type_list,
      ~ grab_aft_output_with_se(
        data.sim,
        time,
        covariate_names,
        data.sim$left_bound,
        data.sim$right_bound,
        .x,
        summary = TRUE
      )
    ) %>% dplyr::bind_rows() %>%
      mutate(iteration = iteration)
  }
}


grab_aft_output_with_se <- function(data.sim, time, covariate_names, left_bound, right_bound, type_list, summary){
  aa <- fit_aft(data.sim, time, covariate_names, left_bound, right_bound, type_list, summary)
  tibble(name = type_list, coef_name = rownames(aa$table), estimate = aa$table[,1], stderr = aa$table[,2])
}

grab_aft_output_without_se <- function(data.sim, time, covariate_names, left_bound, right_bound, type_list, summary){
  aa <- fit_aft(data.sim, time, covariate_names, left_bound, right_bound, type_list, summary)
  tibble(name = type_list, coef_name = rownames(aa$table), estimate =  aa$table[,1])
}





