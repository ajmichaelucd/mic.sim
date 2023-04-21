library(ggplot2)
library(readxl)
library(forcats)
setwd("~/Desktop/Dissertation Project/Chapter 1/mic.sim")
brd <- read_excel("~/Desktop/mar_2023/BRD_Antibiogram_MICs.xlsx")

brd_mh <- brd %>% select(Bug, Drug, MIC, percent_mh) %>% filter(!is.na(percent_mh))
brd_pm <- brd %>% select(Bug, Drug, MIC, percent_pm) %>% filter(!is.na(percent_pm))

mh <- brd_mh %>% mutate(count_mh = 97 * percent_mh / 100,
             count_mh = round(count_mh, digits = 0)) %>% tidyr::uncount(weights = c(count_mh)) %>% select(!c(percent_mh, count_mh)) %>% mutate(mic =
                                                                                     import_mics(MIC)) %>% mutate(
                                                                                       left_bound = mic$left_bound,
                                                                                       right_bound = mic$right_bound,
                                                                                       mic = mic$mic_column
                                                                                     ) %>% select(!MIC) %>% mutate(Bug = "MH")

pm <- brd_pm %>% mutate(count_pm = 97 * percent_pm / 100,
                        count_pm = round(count_pm, digits = 0)) %>% tidyr::uncount(weights = c(count_pm)) %>% select(!c(percent_pm, count_pm)) %>% mutate(mic =
                                                                                     import_mics(MIC)) %>% mutate(
                                                                                       left_bound = mic$left_bound,
                                                                                       right_bound = mic$right_bound,
                                                                                       mic = mic$mic_column
                                                                                     ) %>% select(!MIC) %>% mutate(Bug = "PM")


brd_mh %>% filter(grepl(">", MIC ) | grepl("≤", MIC )) %>% group_by(Drug, MIC) %>% tally %>% select(-n) %>%  print(n = 24)

plot_mics <- function(data,
                      mic_column = "mic",
min = "≤0.25",
max = ">8",
drug = "Ceftiofur"){
data %>% rename(MIC = match(paste0(mic_column), names(data)))

level_list = c(min, 2^(round(log2(parse_number(min) * 2) ): round(log2(parse_number(max)))), max)

data %>% filter(Drug == drug) %>% mutate(mic = factor(mic, levels = level_list)) %>% mutate(censoring = case_when(
  grepl(">", mic) ~ "right",
  grepl("≤", mic) ~ "left",
  grepl("<=", mic) ~ "left",
  TRUE ~ "interval"
)) %>% ggplot() +
  geom_bar(aes(x = mic, fill = censoring)) +
  theme_light() +
#  scale_fill_discrete(drop = FALSE) +
  scale_fill_manual(values = c("left" = "cadetblue3",
                               "interval" = "darkolivegreen3",
                               "right" = "tomato2"), drop = FALSE) +
  scale_x_discrete(drop=FALSE)
}





plot_mics(data = mh, mic_column = "mic", min = "≤0.25", max = ">16", drug = "Ampicillin")
plot_mics(data = mh, mic_column = "mic", min = "≤0.25", max = ">8", drug = "Ceftiofur")
plot_mics(data = mh, mic_column = "mic", min = "≤0.12", max = ">1", drug = "Danofloxacin")
plot_mics(data = mh, mic_column = "mic", min = "≤0.12", max = ">2", drug = "Enrofloxacin")
plot_mics(data = mh, mic_column = "mic", min = "≤0.25", max = ">8", drug = "Florfenicol")
plot_mics(data = mh, mic_column = "mic", min = "≤1", max = ">16", drug = "Gamithromycin") #fm
plot_mics(data = mh, mic_column = "mic", min = "≤0.12", max = ">8", drug = "Penicillin")
plot_mics(data = mh, mic_column = "mic", min = "≤8", max = ">64", drug = "Spectinomycin")
plot_mics(data = mh, mic_column = "mic", min = "≤0.5", max = ">8", drug = "Tetracycline")
plot_mics(data = mh, mic_column = "mic", min = "≤1", max = ">16", drug = "Tildipirosin")
plot_mics(data = mh, mic_column = "mic", min = "≤2", max = ">16", drug = "Tilmicosin")
plot_mics(data = mh, mic_column = "mic", min = "≤8", max = ">64", drug = "Tulathromycin")

plot_mics(data = pm, mic_column = "mic", min = "≤0.25", max = ">16", drug = "Ampicillin")
plot_mics(data = pm, mic_column = "mic", min = "≤0.25", max = ">8", drug = "Ceftiofur")
plot_mics(data = pm, mic_column = "mic", min = "≤0.12", max = ">1", drug = "Danofloxacin")
plot_mics(data = pm, mic_column = "mic", min = "≤0.12", max = ">2", drug = "Enrofloxacin")
plot_mics(data = pm, mic_column = "mic", min = "≤0.25", max = ">8", drug = "Florfenicol") #fm
plot_mics(data = pm, mic_column = "mic", min = "≤1", max = ">8", drug = "Gamithromycin")
plot_mics(data = pm, mic_column = "mic", min = "≤0.12", max = ">8", drug = "Penicillin")
plot_mics(data = pm, mic_column = "mic", min = "≤8", max = ">64", drug = "Spectinomycin")
plot_mics(data = pm, mic_column = "mic", min = "≤0.5", max = ">8", drug = "Tetracycline")
plot_mics(data = pm, mic_column = "mic", min = "≤1", max = ">16", drug = "Tildipirosin")
plot_mics(data = pm, mic_column = "mic", min = "≤2", max = ">16", drug = "Tilmicosin")
plot_mics(data = pm, mic_column = "mic", min = "≤8", max = ">64", drug = "Tulathromycin") #Error
#mh %>% mutate(mic = fct_reorder(mic, left_bound)) %>% filter(Drug == "Ceftiofur") %>% ggplot() +
#  geom_bar(aes(x = mic))






mh_con_key <- brd_mh %>% filter(grepl(">", MIC ) | grepl("≤", MIC )) %>% group_by(Drug, MIC) %>% tally %>% select(-n) %>%  mutate(side = case_when(
  grepl(">", MIC) ~ "high_con",
  grepl("≤", MIC) ~ "low_con",
  grepl("<=", MIC) ~ "low_con",
  TRUE ~ "Error"
),
con = parse_number(MIC)) %>% pivot_wider(id_cols = Drug, names_from = side, values_from = con)


pm_con_key <- brd_pm %>% filter(grepl(">", MIC ) | grepl("≤", MIC )) %>% group_by(Drug, MIC) %>% tally %>% select(-n) %>%  mutate(side = case_when(
  grepl(">", MIC) ~ "high_con",
  grepl("≤", MIC) ~ "low_con",
  grepl("<=", MIC) ~ "low_con",
  TRUE ~ "Error"
),
con = parse_number(MIC)) %>% pivot_wider(id_cols = Drug, names_from = side, values_from = con)






mh %>%
  filter(Drug == "Gamithromycin") %>%
  rowwise %>%
  mutate(t = runif(1,0,1)) %>%
  ungroup %>% left_join(., mh_con_key) %>%
  prep_sim_data_for_em(data.sim = ., scale = "MIC", time = "t") %>%
  fit_model(   formula = Surv(time = left_bound,
                              time2 = right_bound,
                              type = "interval2") ~ 0 + c + strata(c) #+ t:c
               )



mh %>%
  filter(Drug == "Tilmicosin") %>%
  rowwise %>%
  mutate(t = runif(1,0,1)) %>%
  ungroup %>% left_join(., mh_con_key) %>%
  prep_sim_data_for_em(data.sim = ., scale = "MIC", time = "t") %>%
  fit_model_safety(   formula = Surv(time = left_bound,
                                     time2 = right_bound,
                                     type = "interval2") ~ 1 #+ t:c
  )



set.seed(5)

pm %>%
  filter(Drug == "Florfenicol") %>%
  rowwise %>%
  mutate(t = runif(1,0,1)) %>%
  ungroup %>% left_join(., mh_con_key) %>%
  prep_sim_data_for_em(data.sim = ., scale = "MIC", time = "t") %>%
  fit_model(   formula = Surv(time = left_bound,
                              time2 = right_bound,
                              type = "interval2") ~ 0 + c + strata(c),
  initial_weighting = 4
  )
#1# 2.99   -1.38    0.169  0.864
#2#  2.99   -1.38    0.169  0.864
#3# 69153.  -0.742   54921.   1.88
#4# 2.99   -1.38    0.169  0.864




