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



save(brd_mh, file = "~/Desktop/sep_2023/brd_mh.Rdata")
save(brd_pm, file = "~/Desktop/sep_2023/brd_pm.Rdata")

drug = "TILDIP"
bug = "mh"
if(bug == "mh"){
  set = brd_mh
} else if(bug == "pm"){
  set = brd_pm
}else {
  set = NA
}


#prep_df(bug, drug, set)

drug = "PENICI"
bug = "mh"
data = brd_mh
date_col = "Date of Isolation"
date_type = "decimal"
first_year = 2007
id = "accession"
breakpoints_full = readxl::read_excel("~/Desktop/sep_2023/brd_breakpoints.xlsx")


log_reg <- function(drug, bug, data, date_col, date_type, first_year, id, breakpoints_full){
  if(date_type == "decimal"){
    df_temp <- data %>% rename(date = date_col) %>%
      mutate(t = decimal_date(date) - first_year) %>%
      suppressWarnings()
  } else if(date_type == "year"){
    df_temp <- data %>% rename(date = date_col) %>%
      mutate(t = as.numeric(date) - first_year) %>%
      suppressWarnings()
  }else{
    errorCondition("pick decimal or year")
  }

paste0(bug, "_s")
paste0(bug, "_r")

breakpoints = breakpoints_full %>%
  select(all_of(c("drug_name",
                  paste0(bug, "_s"),
                  paste0(bug, "_r")))) %>%
  rename(s_breakpoint = 2, r_breakpoint = 3) %>%
  filter(drug_name == drug)

breakpoints

  import_mics((data %>% select(all_of(drug))) %>%
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
    ) %>%
    #summarise(.by = sir, n = n()) %>%
    #right_join(., tibble(sir = c("S", "I", "R")), by = join_by(sir)) %>%
    #mutate(n = case_when(
    #  !is.na(n) ~ n,
    #  TRUE ~ 0
    #)) %>% arrange(match(sir, c("S", "I", "R")))
    mutate(dichot_res = case_when(
      sir %in% c("I", "R") ~ 1,
      TRUE ~ 0
    )) %>%
    gam::gam(formula = dichot_res ~ lo(t), family = binomial(link = "logit")) %>% return()
}

lr_output = breakpoints_full %>%
  select(all_of(c("drug_name",
                  paste0(bug, "_s"),
                  paste0(bug, "_r")))) %>%
  rename(s_breakpoint = 2, r_breakpoint = 3) %>%
  filter(!is.na(s_breakpoint) & !is.na(r_breakpoint)) %>% pull(drug_name) %>%
  purrr::map(., ~log_reg(.x, bug, data, date_col, date_type, first_year, id, breakpoints_full))

names(lr_output) <- breakpoints_full %>%
  select(all_of(c("drug_name",
                  paste0(bug, "_s"),
                  paste0(bug, "_r")))) %>%
  rename(s_breakpoint = 2, r_breakpoint = 3) %>%
  filter(!is.na(s_breakpoint) & !is.na(r_breakpoint)) %>% pull(drug_name)

names(lr_output)


ggplot() +
  geom_function(fun = function(t){(1 - predict(lr_output$GAMITH, newdata = data.frame(t = t), type = "response"))}, aes(color = "Component 1")) +
  geom_function(fun = function(t){predict(lr_output$GAMITH, newdata = data.frame(t = t), type = "response")}, aes(color = "Component 2")) +
  xlim(0, 16) +
  ylim(0,1)

lr_plot = function(drug, bug){
  ggplot() +
    geom_function(fun = function(t){(1 - predict(lr_output[[drug]], newdata = data.frame(t = t), type = "response"))}, aes(color = "Component 1")) +
    geom_function(fun = function(t){predict(lr_output[[drug]], newdata = data.frame(t = t), type = "response")}, aes(color = "Component 2")) +
    xlim(0, 16) +
    ylim(0,1) +
    ggtitle(paste0(drug, "-", bug, " logistic regression"))
}


purrr::map(names(lr_output), ~lr_plot(.x, bug))




#drug = "PENICI"
bug = "mh"
data = brd_mh
date_col = "Date of Isolation"
date_type = "decimal"
first_year = 2007
id = "accession"
breakpoints_full = readxl::read_excel("~/Desktop/sep_2023/brd_breakpoints.xlsx")

lr_output = breakpoints_full %>%
  select(all_of(c("drug_name",
                  paste0(bug, "_s"),
                  paste0(bug, "_r")))) %>%
  rename(s_breakpoint = 2, r_breakpoint = 3) %>%
  filter(!is.na(s_breakpoint) & !is.na(r_breakpoint)) %>% pull(drug_name) %>%
  purrr::map(., ~log_reg(.x, bug, data, date_col, date_type, first_year, id, breakpoints_full))

names(lr_output) <- breakpoints_full %>%
  select(all_of(c("drug_name",
                  paste0(bug, "_s"),
                  paste0(bug, "_r")))) %>%
  rename(s_breakpoint = 2, r_breakpoint = 3) %>%
  filter(!is.na(s_breakpoint) & !is.na(r_breakpoint)) %>% pull(drug_name)

#purrr::map(names(lr_output), ~lr_plot(.x, bug))


pdf("~/Desktop/sep_2023/brd_mh_logreg.pdf", width=11, height=8.5)
purrr::map(names(lr_output), ~lr_plot(.x, bug))
dev.off()

pdf("~/Desktop/sep_2023/brd_pm_logreg.pdf", width=11, height=8.5)
purrr::map(names(lr_output), ~lr_plot(.x, bug))
dev.off()





