#' Title
#'
#' @param visible_data
#'
#' @return
#' @keywords internal
#'
#' @examples
initial_weighting_staggered_weighting_by_distance_from_median_and_boundary  = function(visible_data){
  median_y = ifelse(median(visible_data$left_bound) < Inf & median(visible_data$left_bound) > -Inf, median(visible_data$left_bound), mean(c(visible_data$low_con[1], visible_data$high_con[1])))
  visible_data %>%
    reframe(.by = everything(),    #implement for other intial weighting options too ##########
            c = as.character(1:2) #fir a logistic regression on c earlier #########
            # `P(C=c|y,t)` = LearnBayes::rdirichlet(1, rep(.1, ncomp)) %>% as.vector(),
            #       .groups = "drop"
    ) %>%
    mutate(
      `P(C=c|y,t)` = case_when(right_bound == Inf & c == "2" ~ 0.9999,
                               right_bound == Inf & c == "1" ~ 0.0001,
                               left_bound > median_y & c == "2" ~ ((((left_bound - median_y) / (high_con - median_y)) * 0.5) + 0.5),
                               left_bound > median_y & c == "1" ~ 1 - ((((left_bound - median_y) / (high_con - median_y)) * 0.5) + 0.5),
                               left_bound <= median_y & left_bound != -Inf & c == "2" ~ 1 - ((((median_y - left_bound) / (median_y - low_con + 1)) * 0.5) + 0.5),
                               left_bound <= median_y & left_bound != -Inf & c == "1" ~ ((((median_y - left_bound) / (median_y - low_con + 1)) * 0.5) + 0.5),
                               left_bound == -Inf & c == "2" ~ 0.0001,
                               left_bound == -Inf & c == "1" ~ 0.9999),
      mid =
        case_when(
          left_bound == -Inf ~ right_bound - 0.5,
          right_bound == Inf ~ left_bound + 0.5,
          TRUE ~ (left_bound + right_bound) / 2
        ),
      rc = ifelse(right_bound == Inf, TRUE, FALSE)
    ) %>% return()
}

initial_weighting_staggered_weighting_by_distance_from_median_and_boundary_plus_random_variation = function(visible_data){
  median_y = ifelse(median(visible_data$left_bound) < Inf & median(visible_data$left_bound) > -Inf, median(visible_data$left_bound), mean(c(visible_data$low_con[1], visible_data$high_con[1])))

  visible_data %>% #visible data with c for component
    reframe(.by = everything(),    #implement for other intial weighting options too ##########
            c = as.character(1:2) #fir a logistic regression on c earlier #########
            # `P(C=c|y,t)` = LearnBayes::rdirichlet(1, rep(.1, ncomp)) %>% as.vector(),
            #       .groups = "drop"
    ) %>%

    mutate(
      `P(C=c|y,t)` = case_when(left_bound > median_y & c == "2" ~ (((left_bound - median_y) / (high_con - median_y)) * 0.5) + 0.5 + (0.05 * sample(c(-1, 0, 1), 1)),
                               left_bound > median_y & c == "1" ~ NaN,
                               left_bound <= median_y & left_bound != -Inf & c == "2" ~ 1 - ((((median_y - left_bound) / (median_y - low_con + 1)) * 0.5) + 0.5) ,
                               left_bound <= median_y & left_bound != -Inf & c == "1" ~ NaN,
                               left_bound == -Inf & c == "2" ~ 0.01,
                               left_bound == -Inf & c == "1" ~ NaN),
      mid =
        case_when(
          left_bound == -Inf ~ right_bound - 0.5,
          right_bound == Inf ~ left_bound + 0.5,
          TRUE ~ (left_bound + right_bound) / 2
        ),
      rc = ifelse(right_bound == Inf, TRUE, FALSE)
    ) %>% ungroup

  possible_data <- possible_data %>% select(obs_id, `P(C=c|y,t)`) %>% rename(prelim = `P(C=c|y,t)`) %>% filter(!is.na(prelim)) %>% right_join(., possible_data, by = "obs_id") %>% mutate(
    `P(C=c|y,t)` = case_when(
      c == "2" ~ prelim,
      c == "1" ~ 1 - prelim,
      TRUE ~ NaN
    )
  ) %>% select(!prelim) %>% return()

}

initial_weighting_flat_interval_censored_full_weight_left_right_censored = function(visible_data){

  visible_data %>% #visible data with c for component
    reframe(.by = everything(),    #implement for other intial weighting options too ##########
            c = as.character(1:2) #fir a logistic regression on c earlier #########
            # `P(C=c|y,t)` = LearnBayes::rdirichlet(1, rep(.1, ncomp)) %>% as.vector(),
            #       .groups = "drop"
    ) %>% #rowwise %>%

    mutate(
      `P(C=c|y,t)` = case_when( left_bound != -Inf & right_bound != Inf & c == "2" ~ 0.5,
                                left_bound != -Inf & right_bound != Inf & c == "1" ~ 0.5,
                                left_bound == -Inf & c == "2" ~ 0,
                                left_bound == -Inf & c == "1" ~ 1,
                                right_bound == Inf & c == "2" ~ 1,
                                right_bound == Inf & c == "1" ~ 0,
                                TRUE ~ NaN),
      mid =
        case_when(
          left_bound == -Inf ~ right_bound - 0.5,
          right_bound == Inf ~ left_bound + 0.5,
          TRUE ~ (left_bound + right_bound) / 2
        ),
      rc = ifelse(right_bound == Inf, TRUE, FALSE)
    ) %>% return()
}

initial_weighting_slight_shift_at_median = function(visible_data){
  median_y = ifelse(median(visible_data$left_bound) < Inf & median(visible_data$left_bound) > -Inf, median(visible_data$left_bound), mean(c(visible_data$low_con[1], visible_data$high_con[1])))

  reframe(.by = everything(),    #implement for other intial weighting options too ##########
          c = as.character(1:2) #fir a logistic regression on c earlier #########
          # `P(C=c|y,t)` = LearnBayes::rdirichlet(1, rep(.1, ncomp)) %>% as.vector(),
          #       .groups = "drop"
  ) %>%

    mutate(
      `P(C=c|y,t)` = case_when(left_bound > median_y & c == "2" ~ 0.55,
                               left_bound > median_y & c == "1" ~ 0.45,
                               left_bound < median_y  & c == "2" ~ 0.45,
                               left_bound < median_y  & c == "1" ~ 0.55,
                               left_bound == median_y ~ 0.5,
                               TRUE ~ NaN),
      mid =
        case_when(
          left_bound == -Inf ~ right_bound - 0.5,
          right_bound == Inf ~ left_bound + 0.5,
          TRUE ~ (left_bound + right_bound) / 2
        ),
      rc = ifelse(right_bound == Inf, TRUE, FALSE)
    ) %>% return()
}

initial_weighting_flat_center_band_of_heavy_weights_at_ends = function(visible_data){
  visible_data %>%
    reframe(.by = everything(),
            c = as.character(1:2)
    ) %>%
    mutate(
      m =  floor(((high_con - low_con) - 1)/ 2),
      `P(C=c|y,t)` = case_when(right_bound == Inf & c == "2" ~ 0.9999,
                               right_bound == Inf & c == "1" ~ 0.0001,
                               left_bound == -Inf & c == "2" ~ 0.0001,
                               left_bound == -Inf & c == "1" ~ 0.9999,
                               low_con + m >= right_bound & c == "2" ~ 0.1,
                               low_con + m >= right_bound & c == "1" ~ 0.9,
                               high_con - m <= left_bound & c == "2" ~ 0.9,
                               high_con - m <= left_bound & c == "1" ~ 0.1,
                               TRUE ~ 0.5),
      mid =
        case_when(
          left_bound == -Inf ~ right_bound - 0.5,
          right_bound == Inf ~ left_bound + 0.5,
          TRUE ~ (left_bound + right_bound) / 2
        ),
      rc = ifelse(right_bound == Inf, TRUE, FALSE)
    ) %>% return()
}

initial_weighting_flat_center_two_bands_of_progressively_heavier_weights_at_ends = function(visible_data){
  visible_data %>% #visible data with c for component
    #   group_by_all() %>%
    reframe(.by = everything(),    #implement for other intial weighting options too ##########
            c = as.character(1:2) #fir a logistic regression on c earlier #########
            # `P(C=c|y,t)` = LearnBayes::rdirichlet(1, rep(.1, ncomp)) %>% as.vector(),
            #       .groups = "drop"
    ) %>%
    #     mutate(
    #     `P(C=c|y,t)` = case_when(left_bound > median_y & c == "1" ~ 0.6,
    #                              left_bound > median_y & c == "2" ~ 0.4,
    #                              left_bound <= median_y & c == "1" ~ 0.4,
    #                              left_bound <= median_y & c == "2" ~ 0.6)
    #     ) %>%
    mutate(
      m =  floor(((high_con - low_con) - 1)/ 2),
      mm = floor(((high_con - low_con))/ 2),
      `P(C=c|y,t)` = case_when(right_bound == Inf & c == "2" ~ 0.9999,
                               right_bound == Inf & c == "1" ~ 0.0001,
                               left_bound == -Inf & c == "2" ~ 0.0001,
                               left_bound == -Inf & c == "1" ~ 0.9999,
                               low_con + m >= right_bound & c == "2" ~ 0.1,
                               low_con + m >= right_bound & c == "1" ~ 0.9,
                               high_con - m <= left_bound & c == "2" ~ 0.9,
                               high_con - m <= left_bound & c == "2" ~ 0.1,
                               mm > m & low_con + mm >= right_bound & c == "2" ~ 0.3,
                               mm > m & low_con + mm >= right_bound & c == "1" ~ 0.7,
                               mm > m & high_con - mm <= left_bound & c == "2" ~ 0.7,
                               mm > m & high_con - mm <= left_bound & c == "1" ~ 0.3,
                               TRUE ~ 0.5),
      mid =
        case_when(
          left_bound == -Inf ~ right_bound - 0.5,
          right_bound == Inf ~ left_bound + 0.5,
          TRUE ~ (left_bound + right_bound) / 2
        ),
      rc = ifelse(right_bound == Inf, TRUE, FALSE)
    ) %>% return()
}
