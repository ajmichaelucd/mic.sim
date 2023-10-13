#' Check Survreg Iteration Maxout
#'
#' Returns a logical value if the number of iterations for survreg to converge is equal to the maximum number of iterations specified
#'
#' @param mu_models list of survreg objects fitted to each component by the EM algorithm
#' @param ncomp number of components being fitted by the EM algorithm
#' @param maxiter_survreg maximum number of iterations for survreg to fit the model
#'
#' @return
#' @export
#'
#' @examples
check_survreg_iteration_maxout = function(mu_models, ncomp, maxiter_survreg){
  purrr::map(1:ncomp, ~(mu_models[[.x]]$iter[1] == maxiter_survreg)) %>% unlist %>% any %>% return()
}
