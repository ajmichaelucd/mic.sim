#' draw_observations
#'
#' draws a single observation from a two-component gaussian mixture model
#'
#' @param year
#'
#' @importFrom mixtools rnormmix
#'
#' @return
#' @export
#'
#' @examples
draw_observations <- function(year = 0, initial_lower_mean = -1, initial_upper_mean = 1, initial_lower_std_dev = 1, initial_upper_std_dev = 1, initial_lower_slp = 0, initial_upper_slp = 0, initial_lower_slp_qd = 0, initial_upper_slp_qd = 0,
                              initial_lower_lambda = 0.5, initial_upper_lambda = 0.5, initial_lower_slp_lambda = 0, initial_upper_slp_lambda = 0){

  rnormmix(1,
           lambda = c(
             initial_lower_lambda + (year * initial_lower_slp_lambda),
             initial_upper_lambda + (year * initial_upper_slp_lambda)
           ),
           mu = c(
             initial_lower_mean + (year * initial_lower_slp) + ((year^2) * initial_lower_slp_qd),
             initial_upper_mean + (year * initial_upper_slp) + ((year^2) * initial_upper_slp_qd)
           ),
           sigma = c(
             initial_lower_std_dev,
             initial_upper_std_dev
           )

  )
}
