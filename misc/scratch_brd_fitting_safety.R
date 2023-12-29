library(ggplotify)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(readxl)
library(ggExtra)


##Imports and Cleaning--------------
brd_breakpoints = readxl::read_excel("~/Desktop/sep_2023/brd_breakpoints.xlsx")
dublin_breakpoints = readxl::read_excel("~/Desktop/sep_2023/dublin_breakpoints.xlsx")

brd_data_mh <-
  readxl::read_excel(
    "~/Desktop/july_2023/BRD MODLING RESULT1.1.xlsx",
    sheet = "M.heam ",
    col_types = c(
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "date",
      "date",
      "text",
      "text",
      "text",
      "skip",
      "text",
      "date",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text"
    )
  )
brd_data_pm <-
  readxl::read_excel(
    "~/Desktop/july_2023/BRD MODLING RESULT1.1.xlsx",
    sheet = "P.mult ",
    col_types = c(
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "date",
      "date",
      "text",
      "text",
      "text",
      "skip",
      "text",
      "date",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text"
    )
  )

dublin_data_gn <- read_excel("~/Desktop/july_2023/dublin_data.xlsx",
                             sheet = "gn")

dublin_data_bopo <-
  read_excel("~/Desktop/july_2023/dublin_data.xlsx",
             sheet = "BOPO Panel")


col.from <- brd_data_mh %>% select(contains("MIC...")) %>% colnames
col.to <-
  brd_data_mh %>% select(-contains("MIC...")) %>% select(-contains("S/I/R")) %>% select(-(1:16)) %>% colnames
brd_mh <-
  brd_data_mh %>%
  select(-all_of(col.to)) %>%
  rename_at(vars(col.from), ~ col.to) %>%
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
  rename_at(vars(col.from), ~ col.to) %>%
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


dublin_gn <-
  dublin_data_gn %>% janitor::clean_names() %>% select(-contains("atb_")) %>% rename(gentamycin_mic = gentamycin_mic_mic)
dublin_bopo <-
  dublin_data_bopo %>% janitor::clean_names() %>% select(-contains("atb_")) %>% rename(enrofloxacin_mic = enrofloxacin_l_mic)
dnames_gn <-
  dublin_gn %>% select(ampicillin_mic:gentamycin_mic) %>% colnames %>% str_remove_all(., "_mic")
dnames_bopo <-
  dublin_bopo %>% select(ampicillin_mic:tylosin_tartrate_mic) %>% colnames %>% str_remove_all(., "_mic")

dublin_gn <-
  dublin_gn %>% rename_at(vars(
    dublin_gn %>% select(ampicillin_mic:gentamycin_mic) %>% colnames
  ), ~ dnames_gn)
dublin_bopo <-
  dublin_bopo %>% rename_at(vars(
    dublin_bopo %>% select(ampicillin_mic:tylosin_tartrate_mic) %>% colnames
  ), ~ dnames_bopo)
























##Modeling-----------
drug = "TULATH"
bug = "pm"
censored_side = "RC"
extra_row = FALSE
if (bug == "mh") {
  set = brd_mh
  s_breakpoint = brd_breakpoints %>% filter(drug_name == drug) %>% pull(mh_s)
  r_breakpoint = brd_breakpoints %>% filter(drug_name == drug) %>% pull(mh_r)
} else if (bug == "pm") {
  set = brd_pm
  s_breakpoint = brd_breakpoints %>% filter(drug_name == drug) %>% pull(pm_s)
  r_breakpoint = brd_breakpoints %>% filter(drug_name == drug) %>% pull(pm_r)
} else if (bug == "dublin_bopo") {
  set = dublin_bopo
  if (drug %in% dublin_breakpoints$drug_name) {
    s_breakpoint = dublin_breakpoints %>% filter(drug_name == drug) %>% pull(bopo_s)
  } else{
    s_breakpoint = NA
  }
  if (drug %in% dublin_breakpoints$drug_name) {
    r_breakpoint = dublin_breakpoints %>% filter(drug_name == drug) %>% pull(bopo_r)
  } else{
    s_breakpoint = NA
  }
} else{
  set = dublin_gn
  if (drug %in% dublin_breakpoints$drug_name) {
    s_breakpoint = dublin_breakpoints %>% filter(drug_name == drug) %>% pull(gn_s)
  } else{
    s_breakpoint = NA
  }
  if (drug %in% dublin_breakpoints$drug_name) {
    r_breakpoint = dublin_breakpoints %>% filter(drug_name == drug) %>% pull(gn_r)
  } else{
    s_breakpoint = NA
  }
}


prep_df(bug, drug, set) -> df_temp

get_concentration = function(df, side) {
  if (side == "low") {
    case_when(
      nrow(df %>% filter(left_bound == -Inf)) == 0 ~ min(df$left_bound),
      TRUE ~ ifelse(
        nrow(df %>% filter(left_bound == -Inf)) == 0,
        0,
        df %>% filter(left_bound == -Inf) %>% pull(right_bound) %>% unique
      )
    ) %>% return()
  } else if (side == "high") {
    case_when(
      nrow(df %>% filter(right_bound == Inf)) == 0 ~ max(df$right_bound),
      TRUE ~ ifelse(
        nrow(df %>% filter(right_bound == Inf)) == 0,
        0,
        df %>% filter(right_bound == Inf) %>% pull(left_bound) %>% unique
      )
    )
  } else{
    errorCondition("choose 'low' or 'high'")
  }
}


low_con <- get_concentration(df_temp, "low")
high_con <- get_concentration(df_temp, "high")

visible_data = df_temp %>%
  mutate(low_con = low_con, high_con = high_con) %>%
  mutate(obs_id = row_number()) %>%
  filter(!is.na(left_bound) & !is.na(right_bound))

prelim_cens_check <- visible_data %>%
  mutate(
    cens = case_when(
      left_bound == -Inf | right_bound == low_con ~ "left_censored",
      right_bound == Inf |
        left_bound == high_con ~ "right_censored",
      TRUE ~ "interval_censored"
    )
  ) %>%
  summarise(.by = cens,
            n = n()) %>%
  mutate(
    n_tot = nrow(visible_data),
    proportion = n / n_tot,
    text_form = paste0("proportion ", cens, " is ", round(proportion, 4))
  )
prelim_cens_check %>% pull(text_form) %>% cat(., sep = "\n")
prelim_cens_check %>% filter(cens != "interval_censored") %>% pull(proportion) %>% sum %>% paste0("total sum of left-censored and right_censored observations is ", .) %>% print()



random_seeds_vector = sample(x = 1:1000000, size = 100, replace = FALSE)
brd_output = EM_fms_surv_batch_run(
  random_seeds_vector,
  visible_data,
  mu_formula = Surv(time = left_bound,
                    time2 = right_bound,
                    type = "interval2") ~ pspline(t, df = 0, caic = TRUE),
  pi_formula = c == "2" ~ s(t),
  censored_side = censored_side,
  extra_row = extra_row,
  max_it = 500,
  ncomp = 2,
  tol_ll = 1e-6,
  pi_link = "logit",
  verbose = 1,
  model_coefficient_tolerance = 0.00001,
  maxiter_survreg = 30,
  sd_initial = 0.2,
  randomize = "all"
)


get_like = function(grid_output){
  grid_output$final_like %>% return()
}

summary = map(brd_output, get_like) %>% data.table::rbindlist(.) %>% tibble %>% arrange(desc(likelihood))
print(summary)

summary %>% summarize(.by = converge, n= n())

summary %>% ggplot() +
  geom_histogram(aes(x = step), binwidth = 1)

summary %>% ggplot() +
  geom_histogram(aes(x = likelihood))


plot_fms(brd_output[[(summary %>% head(1) %>% pull(iter))]]$output, cens_dir = censored_side, title = "top iter")
plot_fms(brd_output[[(summary %>% tail(1) %>% pull(iter))]]$output, cens_dir = censored_side, title = "bottom iter")



get_sigma_init = function(grid_output){
  tibble(c1_scale_init = grid_output$output$possible_data %>% filter(c == "1") %>% pull(sigma_initial) %>% unique,
         c2_scale_init = grid_output$output$possible_data %>% filter(c == "2") %>% pull(sigma_initial) %>% unique,
         grid_output$final_like,
         sd_initial = grid_output$output$sd_initial)
}


map(brd_output, get_sigma_init) %>%
  data.table::rbindlist(.) %>%
  tibble %>%
  arrange(desc(likelihood)) %>%
  ggplot() +
  geom_point(aes(x = c1_scale_init, y = likelihood, color = factor(sd_initial)))

map(brd_output, get_sigma_init) %>%
  data.table::rbindlist(.) %>%
  tibble %>%
  arrange(desc(likelihood)) %>%
  ggplot() +
  geom_point(aes(x = c2_scale_init, y = likelihood, color = factor(sd_initial)))

get_sigma_final = function(grid_output){
  tibble(c1_scale_init = grid_output$output$possible_data %>% filter(c == "1") %>% pull(sigma_initial) %>% unique,
         c2_scale_init = grid_output$output$possible_data %>% filter(c == "2") %>% pull(sigma_initial) %>% unique,
         c1_scale_final = grid_output$output$possible_data %>% filter(c == "1") %>% pull(`sd[Y|t,c]`) %>% unique,
         c2_scale_final = grid_output$output$possible_data %>% filter(c == "2") %>% pull(`sd[Y|t,c]`) %>% unique,
         grid_output$final_like,
         sd_initial = grid_output$output$sd_initial) %>% return()
}


map(brd_output, get_sigma_final) %>%
  data.table::rbindlist(.) %>%
  tibble %>%
  arrange(desc(likelihood)) %>%
  ggplot() +
  geom_point(aes(x = likelihood, y = c1_scale_final, color = factor(sd_initial)))


map(brd_output, get_sigma_final) %>%
  data.table::rbindlist(.) %>%
  tibble %>%
  arrange(desc(likelihood)) %>%
  ggplot() +
  geom_point(aes(x = likelihood, y = c2_scale_final, color = factor(sd_initial)))



















plot_fms(brd_output[[(summary %>% head(10) %>% tail(1) %>% pull(iter))]]$output, cens_dir = censored_side, title = paste0(drug, " ", str_to_upper(bug), " FMS"), add_log_reg = TRUE, s_breakpoint = s_breakpoint, r_breakpoint = r_breakpoint)


plot_fms(brd_output[[(summary %>% head(1) %>% pull(iter))]]$output, cens_dir = censored_side, title = paste0(drug, " ", str_to_upper(bug), " FMS"), add_log_reg = TRUE, s_breakpoint = s_breakpoint, r_breakpoint = r_breakpoint)
