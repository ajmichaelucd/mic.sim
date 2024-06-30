#' Import MICs and Covariates
#'
#' @param data Data frame containing the MICs and covariates
#' @param mic_column String, name of column in data corresponding to the MIC values
#' @param metadata_columns Vector of column names (as strings) for covariates to be included in the data frame produced
#' @param code_column String, name of column containing any symbols for MICs (if data is in a 2 column format)
#' @param combination_agent Numerical, if the MIC is not a combination agent, 0. If combination agent must be separated by '/', use 1 to select the value before the '/', or 2 for the value after. Recommend a value where log2(value) is an integer.
#' @param log_reg_value Logical, TRUE if a column for logistic regression model should be included in output (MICs with `>` will be doubled, MICs with `<`, `<=`, or `≤` are halved)
#' @param scale String, "log" if MIC value should be converted to log2 scale (preferred for subsequent fitting of the model using the EM algorithm)
#' @param round Set to true if log2(MIC values) are integers, but decimal MIC values are rounded (e.g. 0.12 in place of 0.125)
#' @param include_mic_bounds Logical, if TRUE includes left and right boundaries of interval on MIC scale (in addition to on log2 scale if scale is "log")
#'
#' @return
#' @export
#'
#' @examples
#'import_mics_with_metadata(data = tibble(MIC_A = c("≤0.12", ">16", 4, 2), t = runif(4, 0, 10)),
#'                          mic_column = "MIC_A",
#'                          metadata_columns = "t",
#'                          log_reg_value = TRUE,
#'                          scale = "log",
#'                          round = TRUE)
#'
#'import_mics_with_metadata(data = tibble(MIC_A = c(0.125, 16, 4, 2), code_A = c("≤", ">", NA, NA), t = runif(4, 0, 10)),
#'                          mic_column = "MIC_A",
#'                          metadata_columns = "t",
#'                          code_column = "code_A",
#'                          log_reg_value = FALSE,
#'                          scale = "log",
#'                          round = FALSE,
#'                          include_mic_bounds = TRUE)
#'
#'import_mics_with_metadata(data = tibble(MIC_A = c("≤10/1", ">80/8", "40/4", "20/2"), t = runif(4, 0, 10)),
#'                          mic_column = "MIC_A",
#'                          metadata_columns = "t",
#'                          combination_agent = 2,
#'                          log_reg_value = FALSE,
#'                          scale = "log",
#'                          round = FALSE)
#'
import_mics_with_metadata = function(data, mic_column, metadata_columns = NULL, code_column = NULL, combination_agent = 0, log_reg_value = FALSE, scale = "log", round = FALSE, include_mic_bounds = FALSE){
  mic_col = data %>% select(all_of(mic_column)) %>% pull() %>% as.character()
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

  df = import_mics(mic_column = mic_col, code_column = code_col, combination_agent = combination_agent, log_reg_value = log_reg_value, scale = scale, round = round, include_mic_bounds = include_mic_bounds) %>%
    mutate(metadata_col)
  if(!is.null(metadata_col)){
    attr(df, "metadata") = TRUE
    attr(df, "metadata_names") = metadata_columns
  }

  df = df %>% mutate(obs_id = row_number()) %>% relocate(obs_id, .before = everything())

  return(df)
}
