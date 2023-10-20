#' Title
#'
#' @param data either possible_data or a set of mic data ready you want to run import_mics on
#' @param data_type use either "possible_data" if passing in a possible_data object or "import" if you want to import mics and run logistic regression
#' @param drug NULL if using possible_data, if importing data should be the name of the column of the mics or a vector of the mic column and the sign colomn
#' @param date_col string, what is the name of the column in the data that corresponds to time of sampling
#' @param date_type string, either "decimal", "date", or "year" use decimal if using t from possible data, date or year if importing mic data and the date column is a date or just a year respectively
#' @param first_year NULL if date_type is "decimal", otherwise a numeric year or decimal year value if using "year" or "date" for date_type respectively
#' @param s_breakpoint string, the breakpoint on the MIC scale for what constitutes a susceptible isolate, e.g. ≤8 (µg/mL, do not incude units)
#' @param r_breakpoint string, the breakpoint on the MIC scale for what constitutes a resistant isolate, e.g. ≥128 (µg/mL, do not incude units)
#'
#' @return
#' @export
#'
#' @examples
log_reg <- function(data, data_type, drug, date_col, date_type, first_year, s_breakpoint, r_breakpoint){
  if(date_type == "decimal"){
    df_temp = data %>% rename(t = date_col)
  }else if(date_type == "date"){
    df_temp = data %>% rename(date = date_col) %>%
      mutate(t = decimal_date(date) - first_year) %>%
      suppressWarnings()
  } else if(date_type == "year"){
    df_temp = data %>% rename(date = date_col) %>%
      mutate(t = as.numeric(date) - first_year) %>%
      suppressWarnings()
  }else{
    errorCondition("pick decimal or year")
  }

  a = case_when(grepl(pattern = "(≤)|(<=)|(=<)", x = s_breakpoint) ~ parse_number(as.character(s_breakpoint)),
                grepl(pattern = "(<)", x = s_breakpoint) & !grepl(pattern = "(≤)|(<=)|(=<)", x = s_breakpoint) ~ parse_number(as.character(s_breakpoint)) - 0.00001,
                TRUE ~ parse_number(as.character(s_breakpoint))
  )

  b = case_when(grepl(pattern = "(≥)|(>=)|(=>)", x = r_breakpoint) ~ parse_number(as.character(r_breakpoint)),
                grepl(pattern = "(>)", x = r_breakpoint) & !grepl(pattern = "(≥)|(>=)|(=>)", x = r_breakpoint) ~ parse_number(as.character(r_breakpoint)) + 0.00001,
                TRUE ~ parse_number(as.character(r_breakpoint))
  )

  breakpoints = c(s_breakpoint = a, r_breakpoint = b)






  if(data_type == "import"){
    df = import_mics((data %>% select(all_of(drug))) %>%
                       pull(drug)) %>%
      tibble(., df_temp) %>%
      filter(!is.na(mic_column)) %>%
      mutate(
        cens = case_when(
          left_bound == 0 ~ "LC",
          right_bound == Inf ~ "RC",
          TRUE ~ "int"
        ),
        mic = case_when(
          cens == "int" ~ parse_number(mic_column),
          cens == "LC" ~ parse_number(mic_column) * 0.5,
          cens == "RC" ~ parse_number(mic_column) * 2,
          TRUE ~ NaN
        )
      ) %>%
      mutate(
        sir = case_when(
          mic >= parse_number(breakpoints[2]) ~ "R",
          mic <= parse_number(breakpoints[1]) ~ "S",
          mic > parse_number(breakpoints[1]) & mic < parse_number(breakpoints[2]) ~ "I",
          TRUE ~ NA_character_
        )
      )
  } else if(data_type == "possible_data"){
    log_breakpoints = log2(breakpoints)
    df = df_temp %>% filter(c == 1) %>%
      mutate(
        cens = case_when(
          left_bound == -Inf ~ "LC",
          right_bound == Inf ~ "RC",
          TRUE ~ "int"
        ),
        mic = case_when(
          cens == "int" ~ right_bound,
          cens == "LC" ~ right_bound - 0.01,
          cens == "RC" ~ left_bound + 0.01,
          TRUE ~ NaN
        )
      ) %>%
      mutate(
        sir = case_when(
          mic >= log_breakpoints[2] ~ "R",
          mic <= log_breakpoints[1] ~ "S",
          mic > log_breakpoints[1] & mic < log_breakpoints[2] ~ "I",
          TRUE ~ NA_character_
        )
      )
  }else{
    errorCondition("choose either import or possible_data for data_type")
  }

  df %>%
    mutate(dichot_res = case_when(
      sir %in% c("I", "R") ~ 1,
      TRUE ~ 0
    )) %>%
    gam::gam(formula = dichot_res ~ lo(t), family = binomial(link = "logit")) %>% return()
}
