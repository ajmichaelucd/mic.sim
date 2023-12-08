#' Title
#'
#' @param visible_data
#' @param censored_side
#' @param extra_row
#'
#' @return
#' @export
#'
#' @examples
initial_weighting_safety = function(visible_data, censored_side, extra_row){
  if(censored_side == "RC" & !extra_row){
    possible_data =
      visible_data %>% #visible data with c for component
      reframe(.by = everything(),    #implement for other intial weighting options too ##########
              c = as.character(1:2) #fir a logistic regression on c earlier #########
              # `P(C=c|y,t)` = LearnBayes::rdirichlet(1, rep(.1, ncomp)) %>% as.vector(),
              #       .groups = "drop"
      ) %>%
      mutate(
        `P(C=c|y,t)` = case_when(right_bound == Inf & c == "1"~ 0.01 ,
                                 right_bound == Inf & c == "2"~ 0.99 ,
                                 right_bound != Inf & c == "1"~ 1 ,
                                 right_bound != Inf & c == "2"~ 0) #could mess with the cutoff to redefine which observations go in the abnormal group, e.g. new cutoff instead of right_bound == Inf could be left_bound  == ?
        ###Also only works with scale == "log"
      )} else if(censored_side == "LC" & !extra_row){ #%>%
        possible_data =
          visible_data %>% #visible data with c for component
          reframe(.by = everything(),    #implement for other intial weighting options too ##########
                  c = as.character(1:2) #fir a logistic regression on c earlier #########
                  # `P(C=c|y,t)` = LearnBayes::rdirichlet(1, rep(.1, ncomp)) %>% as.vector(),
                  #       .groups = "drop"
          ) %>%
          mutate(
            `P(C=c|y,t)` = case_when(right_bound == low_con & c == "1"~ 0.99 ,
                                     right_bound == low_con & c == "2"~ 0.01 ,
                                     right_bound != low_con & c == "1"~ 0 ,
                                     right_bound != low_con & c == "2"~ 1) #could mess with the cutoff to redefine which observations go in the abnormal group, e.g. new cutoff instead of right_bound == Inf could be left_bound  == ?
            ###Also only works with scale == "log"
          )}else if(censored_side == "LC" & extra_row){ #%>%
            possible_data =
              visible_data %>% #visible data with c for component
              reframe(.by = everything(),    #implement for other intial weighting options too ##########
                      c = as.character(1:2) #fir a logistic regression on c earlier #########
                      # `P(C=c|y,t)` = LearnBayes::rdirichlet(1, rep(.1, ncomp)) %>% as.vector(),
                      #       .groups = "drop"
              ) %>%
              mutate(
                `P(C=c|y,t)` = case_when((right_bound == low_con | left_bound == low_con) & c == "1"~ 0.99 ,
                                         (right_bound == low_con | left_bound == low_con) & c == "2"~ 0.01 ,
                                         right_bound != low_con & left_bound != low_con & c == "1"~ 0 ,
                                         right_bound != low_con & left_bound != low_con & c == "2"~ 1) #could mess with the cutoff to redefine which observations go in the abnormal group, e.g. new cutoff instead of right_bound == Inf could be left_bound  == ?
                ###Also only works with scale == "log"
              )}else if(censored_side == "RC" & extra_row){
                possible_data =
                  visible_data %>% #visible data with c for component
                  reframe(.by = everything(),    #implement for other intial weighting options too ##########
                          c = as.character(1:2) #fir a logistic regression on c earlier #########
                          # `P(C=c|y,t)` = LearnBayes::rdirichlet(1, rep(.1, ncomp)) %>% as.vector(),
                          #       .groups = "drop"
                  ) %>%
                  mutate(
                    `P(C=c|y,t)` = case_when((right_bound == Inf | right_bound == high_con) & c == "1"~ 0.01 ,
                                             (right_bound == Inf | right_bound == high_con) & c == "2"~ 0.99 ,
                                             right_bound != Inf & right_bound != high_con & c == "1"~ 1 ,
                                             right_bound != Inf & right_bound != high_con & c == "2"~ 0) #could mess with the cutoff to redefine which observations go in the abnormal group, e.g. new cutoff instead of right_bound == Inf could be left_bound  == ?
                    ###Also only works with scale == "log"
                  )}else{
                    TRUE ~ errorCondition("censored_side must be 'RC' or 'LC' and extra_row must be logical")
                  }
  possible_data %>% return()
}
