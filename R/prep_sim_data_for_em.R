#' Intermediate function to prepare simulated data for use in EM algorithm
#'
#' @param data.sim
#' @param left_bound_name
#' @param right_bound_name
#' @param time
#' @param covariate_names
#' @param scale
#' @param observed_value_choice
#' @param observed_vale_name
#' @param low_con_name
#' @param high_con_name
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr select all_of mutate n relocate
#'
#' @examples
prep_sim_data_for_em <- function(
    data.sim = simulate_mics(),
    left_bound_name = "left_bound",
    right_bound_name = "right_bound",
    time = "t",
    covariate_names = NULL,
    scale = NULL,
    observed_value_choice = FALSE,
    observed_value_name = "observed_value",
    low_con_name = "low_con",
    high_con_name = "high_con"
) {

if(observed_value_choice){
truth <- data.sim %>% rename(observed_value = match(paste0(observed_value_name), names(data.sim))) %>% select("observed_value")
}

if(is.null(scale) && attr(data.sim, "scale") == "MIC")  {
    df <- data.sim %>%
      select(all_of(c(covariate_names, time)), left_bound = all_of(left_bound_name), right_bound = all_of(right_bound_name), low_con = all_of(low_con_name), high_con = all_of(high_con_name)) %>%
      mutate(obs_id = 1:n(),
             left_bound = log2(left_bound),
             right_bound = log2(right_bound)) %>%
      relocate(obs_id, .before = everything())
    if(observed_value_choice){
      df <- cbind(df, truth) %>% tibble()
    }
  }
  else if (is.null(scale) && attr(data.sim, "scale") == "log"){
    df <- data.sim %>%
      select(all_of(c(covariate_names, time)), left_bound = all_of(left_bound_name), right_bound = all_of(right_bound_name), low_con = all_of(low_con_name), high_con = all_of(high_con_name)) %>%
      mutate(obs_id = 1:n()) %>%
      relocate(obs_id, .before = everything())
    if(observed_value_choice){
      df <- cbind(df, truth) %>% tibble()
    }
  }
  else if(scale == "MIC"){
    df <- data.sim %>%
    select(all_of(c(covariate_names, time)), left_bound = all_of(left_bound_name), right_bound = all_of(right_bound_name), low_con = all_of(low_con_name), high_con = all_of(high_con_name)) %>%
    mutate(obs_id = 1:n(),
           left_bound = log2(left_bound),
           right_bound = log2(right_bound)) %>%
    relocate(obs_id, .before = everything())
    if(observed_value_choice){
      df <- cbind(df, truth) %>% tibble()
    }
}
else if (scale == "log"){
  df <- data.sim %>%
      select(all_of(c(covariate_names, time)), left_bound = all_of(left_bound_name), right_bound = all_of(right_bound_name), low_con = all_of(low_con_name), high_con = all_of(high_con_name)) %>%
      mutate(obs_id = 1:n()) %>%
      relocate(obs_id, .before = everything())
  if(observed_value_choice){
    df <- cbind(df, truth) %>% tibble()
  }
  }
  else{warningCondition(message = "Either (A.) set scale variable to MIC or log or (B.) data.sim should have a scale attribute of either MIC or log")}

return(df)

  }
