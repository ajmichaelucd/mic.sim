#' Title
#'
#' @param results
#' @param settings
#'
#' @return
#' @export
#'
#' @examples
check_directionality <-
  function(results = results,
           settings = settings) {
    df <-
      tibble(t = seq(
        get_t_min_max(settings = settings) %>% pull(t_min),
        get_t_min_max(settings = settings) %>% pull(t_max),
        length.out = 1000
      )) %>%
      mutate(
        c1 = predict(results$single_model_output$single_model_output_fm$newmodel[[1]], newdata = tibble(t)),
        c2 = predict(results$single_model_output$single_model_output_fm$newmodel[[2]], newdata = tibble(t)),
        flip = case_when(c1 > c2 ~ "flip",
                         c2 > c1 ~ "no flip",
                         c2 == c1 ~ "equal",
                         TRUE ~ NA)
      ) %>%
      group_by(flip) %>%
      summarise(n = n())

    if (nrow(df) == 1) {
      flip_decision <- df %>% pull(flip)
      cross <- "no cross"
      directionality <- df %>%
        pivot_wider(names_from = flip, values_from = n) %>%
        mutate(flip_decision = flip_decision,
               cross = cross)

    } else{
      directionality <-
        df %>%
        pivot_wider(names_from = flip, values_from = n) %>%
        mutate(
          flip_decision = case_when(
            flip > `no flip` ~ "flip",
            flip <= `no flip` ~ "no flip",
            TRUE ~ "Error"
          ),
          cross = "cross"
        )
    }
    return(directionality)
  }
