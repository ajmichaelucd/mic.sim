import_mics_v2 = function(mic_column, code_column = NULL, combination_agent = NULL, log_reg_value = FALSE, scale = "log", round = FALSE){

  if(!is.null(combination_agent)){
    mic_column = stringr::str_split_i(mic_column, "/", combination_agent)
  }
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

  if(scale == "log"){
    df = df %>% mutate(
    left_bound_mic = left_bound,
    right_bound_mic = right_bound,
    left_bound = log2(left_bound),
    right_bound = log2(right_bound)
    ) %>% relocate(all_of(c("left_bound", "right_bound")), .before = everything())
  }

  if(scale == "log" & round){
    df = df %>%
      mutate(
        left_bound = round(left_bound),
        right_bound = round(right_bound)
      ) %>%
      relocate(all_of(c("left_bound", "right_bound")), .before = everything())
  }


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
  attr(df, "lr_col") <- log_reg_value
  attr(df, "mic_class") <- "imported_mic_column"
  attr(df, "metadata") <- FALSE
  attr(df, "scale") <- scale
  return(df)
}





import_mics_with_metadata_v2 = function(data, mic_column, metadata_columns = NULL, code_column = NULL, combination_agent = FALSE, log_reg_value = FALSE, scale = "log", round = FALSE){
  mic_col = data %>% select(all_of(mic_column)) %>% pull()
  if(!is.null(metadata_columns)){
    metadata_col = data %>% select(all_of(metadata_columns))
  }else{
    metadata_col = NULL
  }

  if(!is.null(code_column)){
    code_col = data %>% select(all_of(code_column)) %>% pull()
  }else{
    code_col = NULL
  }

  df = import_mics(mic_column = mic_col, code_column = code_col, combination_agent = combination_agent, log_reg_value = log_reg_value, scale = scale, round = round) %>%
    mutate(metadata_col)
  if(!is.null(metadata_col)){
    attr(df, "metadata") = TRUE
    attr(df, "metadata_names") = metadata_columns
  }
  return(df)
}

