#' import_mics
#'
#' @param mic_column
#' @param code_column
#' @param combination_agent
#'
#' @return
#' @export
#'
#' @importFrom dplyr mutate case_when
#' @importFrom magrittr %>%
#' @importFrom stringr str_remove_all
#' @importFrom readr parse_number
#'
#' @examples
import_mics = function(mic_column, code_column = NULL, combination_agent = FALSE){

  df_temp <- dplyr::tibble(mic_column = dplyr::case_when(
    combination_agent == FALSE ~ mic_column,
    TRUE ~ gsub("/.*$", "", mic_column)
  ),
  code_column)

  if(is.null(code_column)){
    df_temp %>%
      mutate(left_bound =
               dplyr::case_when(
                 grepl(pattern = "(≤)|(<=)|(=<)", x = mic_column) ~ 0,
                 grepl(pattern = ">", x = mic_column) ~ readr::parse_number(mic_column),
                 TRUE ~ readr::parse_number(mic_column)/2
               ),
            right_bound =
              dplyr::case_when(
                grepl(pattern = "(≤)|(<=)|(=<)", x = mic_column) ~ stringr::str_remove_all(mic_column, "[≤<=]"),
                grepl(pattern = ">", x = mic_column) ~ "Inf",
                TRUE ~ mic_column
                ) %>% as.numeric()
            )

  } else{
    df_temp %>%
      mutate(left_bound =
               dplyr::case_when(
                 grepl(pattern = "(≤)|(<=)|(=<)", x = code_column) ~ 0,
                 TRUE ~ readr::parse_number(mic_column)/2
               ),
             right_bound =
               dplyr::case_when(
                 grepl(pattern = ">", x = code_column) ~ Inf,
                 TRUE ~ readr::parse_number(mic_column)
               )
      )
  }
}


