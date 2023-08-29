library(ggplotify)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(readxl)
library(ggExtra)

brd_data_mh <- readxl::read_excel("~/Desktop/july_2023/BRD MODLING RESULT1.1.xlsx",
                                  sheet = "M.heam ", col_types = c("text",
                                                                   "text", "text", "text", "text", "text",
                                                                   "text", "text", "text", "date", "date",
                                                                   "text", "text", "text", "skip",
                                                                   "text", "date", "text", "text", "text",
                                                                   "text", "text", "text", "text", "text",
                                                                   "text", "text", "text", "text", "text",
                                                                   "text", "text", "text", "text", "text",
                                                                   "text", "text", "text", "text", "text",
                                                                   "text", "text", "text", "text", "text",
                                                                   "text", "text", "text", "text", "text",
                                                                   "text", "text", "text", "text", "text",
                                                                   "text", "text", "text", "text", "text",
                                                                   "text", "text", "text", "text", "text",
                                                                   "text", "text", "text", "text", "text",
                                                                   "text", "text", "text", "text"))
brd_data_pm <- readxl::read_excel("~/Desktop/july_2023/BRD MODLING RESULT1.1.xlsx",
                                  sheet = "P.mult ", col_types = c("text",
                                                                   "text", "text", "text", "text", "text",
                                                                   "text", "text", "text", "date", "date",
                                                                   "text", "text", "text", "skip",
                                                                   "text", "date", "text", "text", "text",
                                                                   "text", "text", "text", "text", "text",
                                                                   "text", "text", "text", "text", "text",
                                                                   "text", "text", "text", "text", "text",
                                                                   "text", "text", "text", "text", "text",
                                                                   "text", "text", "text", "text", "text",
                                                                   "text", "text", "text", "text", "text",
                                                                   "text", "text", "text", "text", "text",
                                                                   "text", "text", "text", "text", "text",
                                                                   "text", "text", "text", "text", "text",
                                                                   "text", "text", "text", "text", "text",
                                                                   "text", "text", "text", "text"))

dublin_data_gn <- read_excel("~/Desktop/july_2023/dublin_data.xlsx",
                             sheet = "gn")

dublin_data_bopo <- read_excel("~/Desktop/july_2023/dublin_data.xlsx",
                               sheet = "BOPO Panel")


col.from <- brd_data_mh %>% select(contains("MIC...")) %>% colnames
col.to <- brd_data_mh %>% select(-contains("MIC...")) %>% select(-contains("S/I/R")) %>% select(-(1:16)) %>% colnames
brd_mh <-
  brd_data_mh %>%
  select(-all_of(col.to)) %>%
  rename_at(vars(col.from), ~col.to) %>%
  select(-contains("S/I/R")) %>%
  mutate(
        source = tolower(`Specimen Source`),
        source =
          case_when(
            grepl("lung", source) ~ "lower",
            grepl("lg", source) ~ "lower",
            grepl("nas", source) ~ "upper",
            grepl("phary", source) ~ "upper",
            grepl("lary", source) ~ "upper",
            grepl("trach", source) ~ "upper",
            grepl("traech", source) ~ "upper",
            grepl("dnps", source) ~ "upper",
            grepl("bronchus", source) ~ "lower",
            TRUE ~ source
          )
      )
brd_pm <-
  brd_data_pm %>%
  select(-all_of(col.to)) %>%
  rename_at(vars(col.from), ~col.to) %>%
  select(-contains("S/I/R")) %>%
  mutate(
        source = tolower(`Specimen Source`),
        source =
          case_when(
            grepl("lung", source) ~ "lower",
            grepl("lg", source) ~ "lower",
            grepl("nas", source) ~ "upper",
            grepl("phary", source) ~ "upper",
            grepl("lary", source) ~ "upper",
            grepl("trach", source) ~ "upper",
            grepl("traech", source) ~ "upper",
            grepl("dnps", source) ~ "upper",
            grepl("bronchus", source) ~ "lower",
            TRUE ~ source
          )
      )


dublin_gn <- dublin_data_gn %>% janitor::clean_names() %>% select(-contains("atb_")) %>% rename(gentamycin_mic = gentamycin_mic_mic)
dublin_bopo <- dublin_data_bopo %>% janitor::clean_names() %>% select(-contains("atb_")) %>% rename(enrofloxacin_mic = enrofloxacin_l_mic)
dnames_gn <- dublin_gn %>% select(ampicillin_mic:gentamycin_mic) %>% colnames %>% str_remove_all(., "_mic")
dnames_bopo <- dublin_bopo %>% select(ampicillin_mic:tylosin_tartrate_mic) %>% colnames %>% str_remove_all(., "_mic")

dublin_gn <- dublin_gn %>% rename_at(vars(dublin_gn %>% select(ampicillin_mic:gentamycin_mic) %>% colnames), ~dnames_gn)
dublin_bopo <- dublin_bopo %>% rename_at(vars(dublin_bopo %>% select(ampicillin_mic:tylosin_tartrate_mic) %>% colnames), ~dnames_bopo)





grab_column <- function(drug, data, date_col, date_type, first_year, id){
  if(date_type == "decimal"){
    df_temp <- data %>% rename(date = date_col) %>%
      mutate(t = decimal_date(date) - first_year) %>%
      suppressWarnings()
  } else if(date_type == "year"){
    df_temp <- data %>% rename(date = date_col) %>% rowwise %>%
      mutate(t = as.numeric(date) + runif(1, -0.35, 0.35)) %>% ungroup %>%
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

prep_df <- function(organism, drug, data){


if(organism %in% c("mh", "pm")){
  df <- grab_column(drug = drug, data = data, date_col = "Date of Isolation", date_type = "decimal", first_year = 2007, id = "accession")
data %>% select(Accession, source) %>% rename(id = Accession) %>% left_join(df, .) %>% return()
} else{
  df <- grab_column(drug = drug, data = data, date_col = "year", date_type = "year", first_year = 1993, id = "unique_id")
  df %>% return()
}

}
prep_df("mh", "CLINDA", brd_mh)







