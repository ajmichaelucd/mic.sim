

#' censor_values
#'
#' adds new columns with left and right censoring indicators, setting up the creation of a Surv object with interval2 format
#'
#' @param observed_value
#' @param scale
#'
#' @return
#' @keywords internal
#'
#' @importFrom dplyr mutate case_when
#' @importFrom magrittr %>%
#'
#' @examples
censor_values <-
  function(
    simulated_obs,
    #low_con = 2^-4,
    #high_con = 2^4,
    #tested_concentrations = log2(low_con):log2(high_con),
    scale = "log"
  )
  {

if(scale == "MIC"){

  df <-
    simulated_obs %>% rowwise %>%
    mutate(tested_concentrations = list(log2(as.numeric(low_cons)):log2(as.numeric(high_cons)))) %>% mutate(
      left_bound = sapply(observed_value, function(x)
        max(tested_concentrations[tested_concentrations < x])),
      right_bound = sapply(observed_value, function(x)
        min(tested_concentrations[tested_concentrations >= x])),
      indicator = dplyr::case_when(
        is.finite(right_bound) & is.finite(left_bound) ~ 3,
        is.finite(right_bound) &
          is.infinite(left_bound) ~ 2,
        is.infinite(right_bound) &
          is.finite(left_bound) ~ 0
      )
    ) %>%
    suppressWarnings()

  df <- df %>%
        mutate(
          left_bound = 2^left_bound,
          right_bound = 2^right_bound
        )

    return(df)
    }

else if(scale == "log"){

  df <-
    simulated_obs %>% rowwise %>%
    mutate(tested_concentrations = list(as.numeric(low_cons):as.numeric(high_cons))) %>% mutate(
      left_bound = sapply(observed_value, function(x)
        max(tested_concentrations[tested_concentrations < x])),
      right_bound = sapply(observed_value, function(x)
        min(tested_concentrations[tested_concentrations >= x])),
      indicator = dplyr::case_when(
        is.finite(right_bound) & is.finite(left_bound) ~ 3,
        is.finite(right_bound) &
          is.infinite(left_bound) ~ 2,
        is.infinite(right_bound) &
          is.finite(left_bound) ~ 0
      )
    ) %>%
    suppressWarnings()

  return(df)
}
    else{warningCondition(message = "Choose scale: MIC or log")}
  }




