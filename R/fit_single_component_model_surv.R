#' Title
#'
#' @param visible_data
#' @param mu_formula
#' @param maxiter_survreg
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples
fit_single_component_model_surv = function(visible_data, mu_formula, maxiter_survreg, verbose){
  possible_data <-
    visible_data %>%
    mutate(#visible data with c for component
      mid =
        case_when(
          left_bound == -Inf ~ right_bound - 0.5,
          right_bound == Inf ~ left_bound + 0.5,
          TRUE ~ (left_bound + right_bound) / 2
        ),
      rc = ifelse(right_bound == Inf, TRUE, FALSE)
    )

  mu_model  <- survival::survreg(
    mu_formula,  ##Make this chunk into an argument of the function
    data = possible_data,
    dist = "gaussian",
    control = survreg.control(maxiter = maxiter_survreg, debug = verbose > 3))

  return(list(possible_data = possible_data,
              mu_model = mu_model,
              converge = "YES",
              ncomp = ncomp,
         likelihood = tibble(step = 1, likelihood = mu_model$loglik[2]))
  )
}
