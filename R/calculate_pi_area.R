#' Title
#'
#' @param results
#' @param settings
#' @param directionality
#' @param analysis
#' @param single_model_output
#'
#' @return
#' @export
#'
#' @examples
calculate_pi_area <- function(results, settings, directionality, analysis, single_model_output) {
  if (analysis %in% c("fm", "fms")){
    tibble(
      t = seq(
        get_t_min_max(settings = settings) %>% pull(t_min),
        get_t_min_max(settings = settings) %>% pull(t_max),
        length.out = 100000
      ),
      width = (
        get_t_min_max(settings = settings) %>% pull(t_max) - get_t_min_max(settings = settings) %>% pull(t_min)
      ) / 100000
    ) %>%
      mutate(
        pi_hat =
          case_when(
            directionality %>% pull(flip_decision) == "flip" ~ 1 - predict(
              single_model_output$binom_model,
              data.frame(t = t),
              type = "response"
            ),
            TRUE ~ predict(
              single_model_output$binom_model,
              data.frame(t = t),
              type = "response"
            )
          ),
        pi_dgm = settings$pi(t) %>% pull("2"),
        diff = pi_dgm - pi_hat,
        area = abs(diff) * width
      ) %>%
      summarise(
        total_area_pi = sum(area),
        avg_diff_pi = mean(diff),
        med_diff_pi = median(diff)
      ) %>% suppressWarnings()
  }else{
    tibble(total_area_pi = NaN,
           avg_diff_pi = NaN,
           med_diff_pi = NaN)
  }

}
