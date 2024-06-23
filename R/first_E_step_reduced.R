#' Title
#'
#' @param initial_weighting
#' @param visible_data
#' @param plot_visuals
#' @param sd_initial
#' @param ncomp
#' @param non_linear_term
#' @param covariates
#' @param pi_formula
#' @param max_it
#' @param tol_ll
#' @param pi_link
#' @param model_coefficient_tolerance
#'
#' @return
#' @export
#'
#' @examples
first_E_step_reduced = function(initial_weighting, visible_data, plot_visuals, sd_initial = 0.2, ncomp = 2, non_linear_term, covariates, pi_formula, max_it, tol_ll, pi_link, model_coefficient_tolerance, fixed_side, extra_row){
  if(initial_weighting == 1){
    possible_data = initial_weighting_reduced(visible_data = visible_data, fixed_side = fixed_side, extra_row = extra_row )
  }else{
    linear_e_step_output = initial_weighting_fit_linear_model_reduced(visible_data, fixed_side, extra_row = extra_row, non_linear_term, covariates, pi_formula, max_it, ncomp, tol_ll, pi_link, verbose, model_coefficient_tolerance, sd_initial)
    possible_data = linear_e_step_output$possible_data
  }


  attr(possible_data, "plot_initial") <- (plot_visuals & initial_weighting == 8)
  possible_data = add_attribute_data(possible_data, model)
  possible_data = modify_bounds(possible_data)
  return(possible_data)
}


initial_weighting_reduced = function(visible_data, fixed_side, extra_row){
  if(fixed_side == "RC" & !extra_row){
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
      )} else if(fixed_side == "LC" & !extra_row){ #%>%
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
          )}else if(fixed_side == "LC" & extra_row){ #%>%
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
              )}else if(fixed_side == "RC" & extra_row){
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
                    TRUE ~ errorCondition("fixed_side must be 'RC' or 'LC' and extra_row must be logical")
                  }
  possible_data %>% return()
}


initial_weighting_fit_linear_model_reduced = function(visible_data, fixed_side, extra_row = extra_row, non_linear_term, covariates, pi_formula, max_it, ncomp, tol_ll, pi_link, verbose, model_coefficient_tolerance, sd_initial){
  mu_formula = write_all_polynomial_formulas(non_linear_term = non_linear_term, degrees = rep(1, ncomp - 1), covariates = covariates)

  visible_data %>%
    EM_algorithm_reduced(
      fixed_side = fixed_side,
      extra_row = extra_row,
      visible_data = .,
      mu_formula = mu_formula,
      pi_formula = pi_formula,
      max_it = max_it,
      ncomp = ncomp,
      tol_ll = tol_ll,
      pi_link = pi_link,
      verbose = verbose,
      model_coefficient_tolerance = model_coefficient_tolerance,
      initial_weighting = 1,
      sd_initial = sd_initial
    ) %>% return()
}

