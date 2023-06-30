#' Title
#'
#' @param location
#' @param incomplete
#' @param number_per_batch
#' @param array_name
#' @param date
#' @param covariate_effect_vector
#' @param covariate_list
#' @param n
#' @param pi
#' @param intercepts
#' @param trends
#' @param sigma
#' @param nyears
#' @param low_con
#' @param high_con
#' @param max_it
#' @param ncomp
#' @param tol_ll
#' @param maxiter_survreg
#' @param verbose
#' @param allow_safety
#' @param cutoff
#'
#' @return
#' @export
#'
#' @importFrom dplyr tibble mutate inner_join group_by case_when pull
#' @importFrom magrittr %>%
#' @importFrom purrr map as_vector map_chr
#' @importFrom gridExtra grid.arrange
#' @importFrom survival Surv survreg
#'
#' @examples
rerun_incomplete_sets <-
  function(  location,
             incomplete,
             number_per_batch,
             array_name,
             date,
             rerun_parameters){
    if(!is.data.frame(incomplete) && incomplete == "All Clear"){print("No reruns needed, skipping to next step")} else{
      Sys.setlocale (locale = "en_US.UTF-8")
      if(!identical(sort(c("10", "1:")), c("1:", "10"))){
        errorCondition("sort error")
      }

      setwd(location)


      #command line arguments------------
      args <- incomplete %>% pull(incomplete) %>% as.vector()
      batch_size <- number_per_batch
      #batch size: 10, so set the subtracted term to be "batch size - 1"
      #parameters-------

      #this set of runs will vary the sd of the upper component and push it closer to the highest tested concentration (2^2)

      run_name <- paste(array_name, date, sep = "_")
      # covariate_effect_vector <- covariate_effect_vector #0 at start is intercept, then add in the desired coefficients for the covariates
      # covariate_list <-  covariate_list
      # covariate_names <- NULL
      # n=n
      #
      # pi1 = if(pi_truth == "identity"){function(t) {z <- pi_int + pi_trend * t #changed to 0.5
      # c("1" = z, "2" = 1- z)}
      #   } else{
      #   function(t) {m <- pi_int + pi_trend * t   #logit
      #   z <- exp(m) / (1+ exp(m))
      #   c("1" = z, "2" = 1 - z)}
      #   }
      #
      # `E[X|T,C]` = function(t, c)
      # {
      #   case_when(
      #     c == "1" ~ intercepts[1] + trends[1] * t,
      #     c == "2" ~ intercepts[2] + trends[2] * t,
      #     TRUE ~ NaN
      #   )
      # }
      #
      # t_dist1 = function(n){runif(n, min = 0, max = nyears)}
      #
      # sd_vector = c("1" = sigma[1], "2" = sigma[2]) #0.5, 0.75, 1, 1.25
      #
      # low_con = low_con
      # high_con = high_con #errored out when this was 2^3
      # #RUN 1 : 2
      # #RUN 2: 3
      # #RUN 3: 4
      #
      # scale = "log"
      #
      # formula = Surv(time = left_bound,
      #                time2 = right_bound,
      #                type = "interval2") ~ 0 + c + strata(c) + t:c

      for(i in args){local_full_run_function(
        args = i,
        batch_size = batch_size,
        run_name = run_name,
        rerun_parameters = rerun_parameters
        )
      }

     # setwd(
     #   "~/Desktop/Dissertation Project/Chapter 1/mic.sim"
     # )
    }

  }

##missing covariate names, need to add conc table too

