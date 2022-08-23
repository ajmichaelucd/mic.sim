#' Intermediate function to prepare simulated data for use in EM algorithm
#'
#' @param data.sim
#' @param left_bound_name
#' @param right_bound_name
#' @param time
#' @param covariate_names
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr select all_of mutate n relocate .before
#'
#' @examples
prep_sim_data_for_em <- function(
    data.sim,
    left_bound_name = "left_bound",
    right_bound_name = "right_bound",
    time = "t",
    covariate_names,
    scale = NULL
) {
if(scale == "MIC"){
  data.sim %>%
    select(all_of(c(covariate_names, time)), left_bound = all_of(left_bound_name), right_bound = all_of(right_bound_name)) %>%
    mutate(obs_id = 1:n(),
           left_bound = log2(left_bound),
           right_bound = log2(right_bound)) %>%
    relocate(obs_id, .before = everything())
}
else if (scale == "log"){
    data.sim %>%
      select(all_of(c(covariate_names, time)), left_bound = all_of(left_bound_name), right_bound = all_of(right_bound_name)) %>%
      mutate(obs_id = 1:n()) %>%
      relocate(obs_id, .before = everything())
  }
else if(is.null(scale) & attr(data.sim, "scale") == "MIC")  {
  data.sim %>%
    select(all_of(c(covariate_names, time)), left_bound = all_of(left_bound_name), right_bound = all_of(right_bound_name)) %>%
    mutate(obs_id = 1:n(),
           left_bound = log2(left_bound),
           right_bound = log2(right_bound)) %>%
    relocate(obs_id, .before = everything())
}
  else if (is.null(scale) & attr(data.sim, "scale") == "log"){
    data.sim %>%
      select(all_of(c(covariate_names, time)), left_bound = all_of(left_bound_name), right_bound = all_of(right_bound_name)) %>%
      mutate(obs_id = 1:n()) %>%
      relocate(obs_id, .before = everything())
  }
  else{warningCondition(message = "Either (A.) set scale variable to MIC or log or (B.) data.sim should have a scale attribute of either MIC or log")}
}
