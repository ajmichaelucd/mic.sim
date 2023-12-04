#' Title
#'
#' @param visible_data
#'
#' @return
#' @export
#'
#' @examples
find_censoring_levels = function(visible_data){

  prelim_cens_check <- visible_data %>%
    mutate(
      cens = case_when(
        left_bound == -Inf | right_bound == low_con ~ "left_censored",
        right_bound == Inf |
          left_bound == high_con ~ "right_censored",
        TRUE ~ "interval_censored"
      )
    ) %>%
    summarise(.by = cens,
              n = n()) %>%
    mutate(
      n_tot = nrow(visible_data),
      proportion = n / n_tot,
      text_form = paste0("proportion ", cens, " is ", round(proportion, 4))
    )
  prelim_cens_check %>% pull(text_form) %>% cat(., sep = "\n")
  prelim_cens_check %>% filter(cens != "interval_censored") %>% pull(proportion) %>% sum %>% paste0("total sum of left-censored and right_censored observations is ", .) %>% print

}
