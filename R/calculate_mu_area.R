#' Title
#'
#' @param results
#' @param settings
#' @param directionality
#' @param analysis
#'
#' @return
#' @export
#'
#' @examples
calculate_mu_area <- function(results, settings, directionality, analysis) {
  if (analysis == "fm") {
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
      reframe(.by = everything(),    #implement for other initial weighting options too ##########
              c = as.character(1:2)) %>%
      mutate(
        mu_hat = case_when(
          (directionality %>% pull(flip_decision) == "no flip") &
            c == "1" ~ predict(results$single_model_output$single_model_output_fm$newmodel[[1]], data.frame(t = t)),
          (directionality %>% pull(flip_decision) == "no flip") &
            c == "2" ~ predict(results$single_model_output$single_model_output_fm$newmodel[[2]], data.frame(t = t)),
          (directionality %>% pull(flip_decision) == "flip") &
            c == "1" ~ predict(results$single_model_output$single_model_output_fm$newmodel[[2]], data.frame(t = t)),
          (directionality %>% pull(flip_decision) == "flip") &
            c == "2" ~ predict(results$single_model_output$single_model_output_fm$newmodel[[1]], data.frame(t = t)),
          TRUE ~ NaN
        ),
        mu_dgm = settings$`E[X|T,C]`(t = t, c = c),
        diff = mu_dgm - mu_hat,
        area = abs(diff) * width
      ) %>%
      group_by(c) %>%
      summarise(
        total_area = sum(area),
        avg_diff = mean(diff),
        med_diff = median(diff)
      ) %>%
      pivot_wider(
        names_from = c,
        names_prefix = "c",
        values_from = total_area:med_diff
      ) %>% return()
  } else if (analysis == "fms") {
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
      reframe(.by = everything(),    #implement for other initial weighting options too ##########
              c = as.character(1:2)) %>%
      mutate(
        mu_hat = case_when(
          c == "1" ~ predict(results$single_model_output$single_model_output_fms$newmodel, data.frame(t = t)),
          c == "2" ~ NaN,
          TRUE ~ NaN
        ),
        mu_dgm = settings$`E[X|T,C]`(t = t, c = c),
        diff = mu_dgm - mu_hat,
        area = abs(diff) * width
      ) %>%
      group_by(c) %>%
      summarise(
        total_area = sum(area),
        avg_diff = mean(diff),
        med_diff = median(diff)
      ) %>%
      pivot_wider(
        names_from = c,
        names_prefix = "c",
        values_from = total_area:med_diff
      ) %>% return()
  } else{
    tibble(
      total_area_c1 = NaN,
      total_area_c2 = NaN,
      avg_diff_c1 = NaN,
      avg_diff_c2 = NaN,
      med_diff_c1 = NaN,
      med_diff_c2 = NaN
    ) %>% return()
  }

}
