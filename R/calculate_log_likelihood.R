#' Title
#'
#' @param possible_data
#' @param verbose
#'
#' @return
#' @keywords internal
#'
#' @examples
calculate_log_likelihood = function(possible_data, verbose = 0){
  log_likelihood_obs <- possible_data %>%
    summarise(.by = obs_id, likelihood_i = sum(`P(c,y|t)`)) %>%
    mutate(log_likelihood_i = log(likelihood_i))
  log_likelihood <- sum(log_likelihood_obs$log_likelihood_i)
  if(verbose > 2){
    message(log_likelihood)
  }
  return(log_likelihood)
}
