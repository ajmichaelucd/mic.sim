#' Title
#'
#' @param data
#'
#' @return
#' @keywords internal
#'
#' @examples
add_obs_id = function(data){
  if(ncol(data %>% select(matches("obs_id"))) == 0){ #add function
    data %>% mutate(obs_id = row_number()) %>% select(obs_id, everything()) %>% return()
  }else{
    return(data)
  }
}
