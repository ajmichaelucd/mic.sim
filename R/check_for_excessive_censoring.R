#' Title
#'
#' @param df
#' @param cutoff
#'
#' @return
#' @export
#'
#' @examples
check_for_excessive_censoring <- function(single_model_output, cutoff = 0.9){
  df1 <-  single_model_output[["possible_data"]] %>%
    mutate(lc = case_when(left_bound == -Inf ~ TRUE,
                          right_bound == low_con ~ TRUE,
                          TRUE ~ FALSE),
           rc = case_when(right_bound == Inf ~ TRUE,
                          left_bound == high_con ~ TRUE,
                          TRUE ~ FALSE))
  # all this could be a function

  cens_test_results <- tibble(c = c("1", "1", "2", "2"), direction = c("rc", "lc", "rc", "lc"), cutoff = cutoff) %>%
    rowwise %>%
    mutate(
      check_weigh_prop_cens(df1, c, direction, cutoff)
    )

  check_for_excess_censor <- case_when(
    nrow(
      cens_test_results %>% filter(direction == "lc" &
                                     decision != "All Clear")
    ) > 0 &
      nrow(
        cens_test_results %>% filter(direction == "rc" &
                                       decision != "All Clear")
      ) > 0 ~ "BOTH",
    nrow(
      cens_test_results %>% filter(direction == "lc" &
                                     decision != "All Clear")
    ) > 0 ~ "LC",
    nrow(
      cens_test_results %>% filter(direction == "rc" &
                                     decision != "All Clear")
    ) > 0 ~ "RC",
    TRUE ~ "All Clear"
  )
  #df2 <- rbind(
  #  check_weigh_prop_cens(df1, "1", "rc", cutoff),
  #  check_weigh_prop_cens(df1, "1", "lc", cutoff),
  #  check_weigh_prop_cens(df1, "2", "rc", cutoff),
  #  check_weigh_prop_cens(df1, "2", "lc", cutoff)
#  #) %>% filter(decision == "Excessive Censoring")
#
#  if(nrow(df2) > 0){
#    check_for_excess_censor <- df2 %>% unique %>% pull()
#  } else{
#    check_for_excess_censor <- "All Clear"
#  }

  return(check_for_excess_censor)
}
