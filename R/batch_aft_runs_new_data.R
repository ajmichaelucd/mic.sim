#' batch_aft_runs_new_data
#'
#' Run single_aft_run_new_data over a specified number of iterations.
#' For each iteration a new data set is generated and all six models are fitted to the data set.
#'
#' @param iterations
#' @param covariate_effect_vector
#' @param covariate_list
#' @param covariate_names
#' @param n
#' @param t_dist
#' @param pi
#' @param `E[X|T,C]`
#' @param sd_vector
#' @param low_con
#' @param high_con
#' @param type_list
#' @param time
#' @param MIC_breakpoint
#' @param stderr
#'
#' @return
#' @export
#'
#' @importFrom dplyr mutate all_of select bind_rows tibble inner_join group_by case_when
#' @importFrom magrittr %>%
#' @importFrom survival Surv survreg
#' @importFrom broom tidy
#' @importFrom purrr map as_vector
#' @importFrom tidyr pivot_wider
#'
#' @examples
batch_aft_runs_new_data <- function(
    iterations = 10,
    covariate_effect_vector = c(0),
    covariate_list = NULL,
    covariate_names = NULL,
    n = 100,
    t_dist = function(n){runif(n, min = 0, max = 1)},
    pi = function(t) {z <- 1 + 0* t
    c("1" = z)},
    `E[X|T,C]` = function(t, c)
    {
      case_when(
        c == "1" ~ 0 + 0 * t,
        TRUE ~ NaN
      )
    },
    sd_vector =  c("1" = 1),
    low_con = 2^-4,
    high_con = 2^4,
    type_list = c("loglogistic", "lognormal", "weibull", "gaussian", "exponential", "logistic"),
    time = "t",
    #summary = TRUE, #summary must be TRUE for the part that grabs coefficients to work, if needed could run an if statement later for this
    MIC_breakpoint = 1,
    stderr = FALSE)
{purrr::map(c(1:iterations), ~single_aft_run_new_data(covariate_effect_vector, covariate_list, covariate_names, n, t_dist, pi, `E[X|T,C]`, sd_vector, low_con, high_con, type_list, time, MIC_breakpoint, iteration = .x, stderr)) %>%
    dplyr::bind_rows() }
