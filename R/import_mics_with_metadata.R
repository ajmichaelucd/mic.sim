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
import_mics_with_metadata = function(mic_column, metadata_cols = NULL, code_column = NULL, combination_agent = FALSE, log_reg_value = FALSE){
df = import_mics(mic_column = mic_column, code_column = code_column, combination_agent = combination_agent, log_reg_value = log_reg_value) %>%
    mutate(metadata_cols)
if(!is.null(metadata_cols)){
  attr(df, "metadata") = TRUE
}
return(df)
}
