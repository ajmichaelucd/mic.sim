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
import_mics = function(mic_column, code_column = NULL, combination_agent = FALSE, log_reg_value = FALSE){

  mic_column <- dplyr::case_when(
    grepl("/", as.character(mic_column)) ~ gsub("/.*$", "", mic_column),
    TRUE ~ as.character(mic_column))
  code_column <- code_column
  df_temp <- dplyr::tibble(mic_column, code_column)



  if(is.null(code_column)){
    df <- df_temp %>%
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
    df <- df_temp %>%
      mutate(left_bound =
               dplyr::case_when(
                 grepl(pattern = "(≤)|(<=)|(=<)", x = code_column) ~ 0,
                 grepl(pattern = ">", x = code_column) ~ readr::parse_number(mic_column),
                 TRUE ~ readr::parse_number(mic_column)/2
               ),
             right_bound =
               dplyr::case_when(
                 grepl(pattern = ">", x = code_column) ~ Inf,
                 TRUE ~ readr::parse_number(mic_column)
               )
      )

  }
 df = df %>%
    mutate(
      left_bound = case_when(
        left_bound == "0.12" ~ 0.125,
        left_bound == "0.06" ~ 2^-4,
        left_bound == "0.03" ~ 2^-5,
        left_bound == "0.015" ~ 2^-6,
        left_bound == "0.0075" ~ 2^-7,
        left_bound == "0.00375" ~ 2^-8,
        TRUE ~ left_bound
      ),
      right_bound = case_when(
        right_bound == "0.12" ~ 0.125,
        right_bound == "0.06" ~ 2^-4,
        right_bound == "0.03" ~ 2^-5,
        right_bound == "0.015" ~ 2^-6,
        right_bound == "0.0075" ~ 2^-7,
        right_bound == "0.00375" ~ 2^-8,
        TRUE ~ right_bound
      )
    )

 if(log_reg_value){
   df = df %>% mutate(
     mic_column = paste0(code_column, mic_column),
     lr_column =
       case_when(
         grepl(pattern = "(≤)|(<=)|(=<)", x = mic_column) ~ parse_number(mic_column),
         grepl(pattern = ">", x = mic_column) ~ parse_number(mic_column) * 2,
         TRUE ~ parse_number(mic_column)
       )
   )
 }

 attr(df, "source") <- "imported"
 attr(df, "lr_col") <- TRUE
 attr(df, "mic_class") <- "imported_mic_column"
 attr(df, "metadata") <- FALSE
 return(df)
}





