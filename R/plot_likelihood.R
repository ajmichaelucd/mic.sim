#' Plot Likelihood Curve
#'
#' Plot likelihood curve of fitted model over the steps of the EM algorithm
#'
#' @param likelihood_documentation tibble of likelihood per step of model fitting, part of the output of `fit_EM()`
#' @param format string, either "tibble" or "matrix". Describes format of likelihood documentation.
#'
#' @import ggplot2
#'
#' @return
#' @export
#'
#' @examples
#' data = simulate_mics()
#' output = fit_EM(model = "pspline",
#' approach = "full",
#' pre_set_degrees = c(4,4),
#' visible_data = data,
#' non_linear_term = "t",
#' covariates = NULL,
#' pi_formula = c == "2" ~ s(t),
#' max_it = 300,
#' ncomp = 2,
#' tol_ll = 1e-6,
#' pi_link = "logit",
#' verbose = 1,
#' model_coefficient_tolerance = 0.00001,
#' initial_weighting = 3,
#' sd_initial = 0.2
#' )
#' plot_likelihood(likelihood_documentation = output$likelihood)
#'
#'
plot_likelihood = function(likelihood_documentation, format = "tibble"){
  if(format == "matrix"){
    like <- likelihood_documentation %>%
      as_tibble %>%
      suppressWarnings() %>%
      rename(step = V1, loglikelihood = V2, survreg_maxout = V3)
    diff = c(NaN, diff(like$loglikelihood))
      like = like %>% tibble(., diff) %>%
      filter(!is.na(loglikelihood))
      }
  else if(format == "tibble"){
    diff = c(NaN, diff(likelihood_documentation$loglikelihood))
    like = likelihood_documentation %>% tibble(., diff) %>%
      filter(!is.na(loglikelihood))
  }

  like %>%
    mutate(
            diff_sign =
              case_when(diff > 0 ~ "inc",
                        is.na(diff) ~ NA,
                        TRUE ~ "dec")
    ) %>%
    ggplot() +
    geom_line(aes(x = step, y = loglikelihood)) +
    geom_point(aes(x = step, y = loglikelihood, color = diff_sign)) +
    theme_minimal() + scale_color_discrete(name = "Change in LL") +
    ylab("log-likelihood")
}
