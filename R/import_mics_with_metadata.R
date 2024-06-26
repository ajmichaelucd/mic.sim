#' Title
#'
#' @param mic_column
#' @param metadata_cols
#' @param code_column
#' @param combination_agent
#' @param log_reg_value
#'
#' @return
#' @export
#'
#' @examples
import_mics_with_metadata = function(data, mic_column, metadata_columns = NULL, code_column = NULL, combination_agent = NULL, log_reg_value = FALSE, scale = "log", round = FALSE){
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

  df = df %>% mutate(obs_id = row_number()) %>% relocate(obs_id, .before = everything())

  return(df)
}
