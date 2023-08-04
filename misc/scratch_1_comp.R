fit_model_pi = function(
    visible_data,

    formula = Surv(time = left_bound,
                   time2 = right_bound,
                   type = "interval2") ~ pspline(t, df = 0, calc = TRUE),
    formula2 = c == "2" ~ s(t), #or: c == "2" ~ lo(t)
    max_it = 3000,
    ncomp = 2,
    tol_ll = 1e-6,
    browse_at_end = FALSE,
    browse_each_step = FALSE,
    plot_visuals = FALSE,
    pi_link = "logit",
    #silent = FALSE,
    verbose = 3,
    #low_con = 2^-3,
    #high_con = 2^3,
    maxiter_survreg = 30,
    initial_weighting = 1 #smoothingspline or loess
){
  #verbose = 0: print nothing
  #verbose = 1: print run number (controlled outside in the purrr::map of this) --done
  #verbose = 2: print run number and iteration number --done
  #verbose = 3: print run number, iteration number, and iteration results --done
  #verbose = 4: print run number, iteration number, iteration results, and run aft as verbose
  #verbose = 0:


  #first E step-----

    possible_data <-
      visible_data %>%
      mutate(#visible data with c for component
        mid =
          case_when(
            left_bound == -Inf ~ right_bound - 0.5,
            right_bound == Inf ~ left_bound + 0.5,
            TRUE ~ (left_bound + right_bound) / 2
          ),
        rc = ifelse(right_bound == Inf, TRUE, FALSE)
      ) #%>%  ##this is probably only accurate for scale = "log"
    #print()

  newmodel  <- survival::survreg(
      formula,  ##Make this chunk into an argument of the function
      data = possible_data,
      dist = "gaussian",
      control = survreg.control(maxiter = maxiter_survreg, debug = verbose > 3))

  return(list(possible_Data = possible_data,
              newmodel = newmodel))

}
