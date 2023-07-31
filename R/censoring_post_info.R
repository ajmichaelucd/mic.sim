#' Title
#'
#' @param possible_data
#' @param settings
#' @param comparison
#'
#' @return
#' @export
#'
#' @examples
censoring_post_info <-
  function(possible_data, settings, comparison = "model_weighted") {
    df <- possible_data %>%
      mutate(
        censor =
          case_when(
            settings$scale == "log" & left_bound == -Inf ~ "left",
            settings$scale == "MIC" &
              (left_bound == 0 | left_bound == -Inf) ~ "left",
            right_bound == Inf ~ "right",
            TRUE ~ "interval"
          )
      )

    if (comparison == "model_weighted") {
      df %>% group_by(c, censor) %>% summarise(p = sum(`P(C=c|y,t)`)) %>%
        mutate(sum = sum(p[c == c]),
               weighted_prop_model = p / sum) %>%
        ungroup() %>%
        select(-c(p, sum)) %>%
        rename(comp = c) %>%
        tidyr::complete(comp, censor, fill = list(weighted_prop_model = 0)) %>%
        return()

    } else if (comparison == "true_pct") {
      df %>% group_by(comp, censor) %>% summarise(p = n()) %>%
        mutate(sum = sum(p[comp == comp]),
               weighted_prop_true = p / sum) %>%
        ungroup() %>%
        select(-c(p, sum)) %>%
        tidyr::complete(comp, censor, fill = list(weighted_prop_true = 0)) %>%
        return()

    } else{
      errorCondition("choose model_weighted or true_pct")
    }

  }
