#' Title
#'
#' @param organism
#' @param drug
#' @param data
#'
#' @return
#' @export
#'
#' @examples
prep_df <- function(organism, drug, data){


  if(organism %in% c("mh", "pm")){
    df <- grab_column(drug = drug, data = data, date_col = "Date of Isolation", date_type = "decimal", first_year = 2007, id = "accession")
    data %>% select(Accession, source) %>% rename(id = Accession) %>% left_join(df, .) %>% return()
  } else{
    df <- grab_column(drug = drug, data = data, date_col = "year", date_type = "year", first_year = 1993, id = "unique_id")
    df %>% return()
  }

}

grab_column <- function(drug, data, date_col, date_type, first_year, id){
  if(date_type == "decimal"){
    df_temp <- data %>% rename(date = date_col) %>%
      mutate(t = decimal_date(date) - first_year) %>%
      suppressWarnings()
  } else if(date_type == "year"){
    df_temp <- data %>% rename(date = date_col) %>%
      mutate(t = as.numeric(date) ) %>%
      suppressWarnings()
  }else{
    errorCondition("pick decimal or year")
  }




  import_mics((data %>% select(all_of(drug))) %>%
                pull(drug)) %>%
    mutate(left_bound = log2(left_bound), right_bound = log2(right_bound)) %>%
    # mutate(
    #   cens =
    #     case_when(
    #       left_bound == -Inf ~ "lc",
    #       right_bound == Inf ~ "rc",
    #       TRUE ~ "int"
    #     ),
    #   mid =
    #     case_when(
    #       left_bound == -Inf ~ right_bound - 0.5,
    #       right_bound == Inf ~ left_bound + 0.5,
  #       TRUE ~ (left_bound + right_bound) / 2
  #     )) %>%
  tibble(., df_temp) -> df
  if(id == "accession"){
    df %>% rename(id = "Accession") %>% relocate(t) %>% relocate(id) -> result
  } else if(id == "unique_id"){
    df %>% rename(id = "unique_id") %>% relocate(t) %>% relocate(id) -> result
  }else{
    df -> result
    print("try accession or unique_id")
  }
  result %>%
    filter(!is.na(left_bound)) %>%
    select(id:right_bound) %>%
    return()
  #
  # if(nrow(df %>% filter(left_bound == -Inf)) > 0){
  #   plot_min <- (df %>% filter(left_bound == -Inf) %>% pull(right_bound) %>% min) - 1
  # }else{
  #   plot_min <- (df %>% pull(left_bound) %>% min) - 1
  # }
  #
  # if(nrow(df %>% filter(right_bound == Inf)) > 0){
  #   plot_max <- (df %>% filter(right_bound == Inf) %>% pull(left_bound) %>% max) + 1
  # }else{
  #   plot_max <- (df %>% pull(right_bound) %>% max) + 1
  # }
  #



}
