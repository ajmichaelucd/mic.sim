
log_reg <- function(data, data_type, drug, date_col, date_type, first_year, id, s_breakpoint, r_breakpoint){
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
        mic >= parse_number(breakpoints$r_breakpoint) ~ "R",
        mic <= parse_number(breakpoints$s_breakpoint) ~ "S",
        mic > parse_number(breakpoints$s_breakpoint) & mic < parse_number(breakpoints$r_breakpoint) ~ "I",
        TRUE ~ NA_character_
      )
    )
  } else if(data_type == "possible_data"){
    log_breakpoints = log2(breakpoints)
    df = df_temp %>%
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
          mic >= breakpoints$r_breakpoint ~ "R",
          mic <= breakpoints$s_breakpoint ~ "S",
          mic > breakpoints$s_breakpoint & mic < breakpoints$r_breakpoint ~ "I",
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
