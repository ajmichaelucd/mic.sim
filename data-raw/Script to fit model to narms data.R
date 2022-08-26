library(readxl)
full_narms <- read.csv("~/Desktop/2021-FSIS-00065-F_3663/FOIA2021-065.csv", na.strings="NULL")
key <- full_narms %>%
  group_by(Genus) %>% group_keys()
split_narms <- full_narms %>%
  group_by(Genus) %>% summarize(n = n())
  group_split()
narms_CA <- full_narms %>% filter(Genus == "Campylobacter")
narms_EC <- full_narms %>% filter(Genus == "E. coli")
narms_EN <- full_narms %>% filter(Genus == "Enterococcus")
narms_SA <- full_narms %>% filter(Genus == "Salmonella")

not_all_na <- function(x) !all(is.na(x)|x==0|x=="<NA>")

narms_CA %>% select(where(not_all_na))

hist(log2(narms_CA$Azithromycin_MIC))
hist(log2(narms_CA$Ciprofloxacin_MIC))
hist(log2(narms_CA$Clindamycin_MIC))
hist(log2(narms_CA$Erythromycin_MIC))
hist(log2(narms_CA$Florfenicol_MIC))
hist(log2(narms_CA$Gentamicin_MIC))
hist(log2(narms_CA$Nalidixic_Acid_MIC))
hist(log2(narms_CA$Telithromycin_MIC))
hist(log2(narms_CA$Tetracycline_MIC))

narms_CA %>% mutate(Species = tolower(Species)) %>% filter(Species %in% c("coli", "jejuni")) %>%
ggplot() +
  geom_histogram(aes(x = log2(Florfenicol_MIC), fill = Species), binwidth = 1)

ca_cip_mics <- import_mics(narms_CA$Florfenicol_MIC, narms_CA$Florfenicol_Operator)

ca_cip <- narms_CA %>%
  select(EstID:Updated_Profile) %>%
  mutate(Species = tolower(Species)) %>%
  tibble(., ca_cip_mics) %>%
  mutate(t = lubridate::decimal_date( lubridate::dmy(Collection_Date)) - 2013) %>%
  filter(Species %in% c("coli"
                        #,
                        #"jejuni"
                        ))

ca_cip %>% group_by(code_column) %>% summarize(pct = 100 * n() / nrow(ca_cip))

prep_sim_data_for_em(ca_cip, covariate_names = c("Sampling_Program", "Animal_Host", "Species"), scale = "MIC") %>%
fit_model(., formula = Surv(time = left_bound, time2 = right_bound, type = "interval2") ~ 0 + c +
            strata(c) + t:c
          #+ t:Species
          #+ Sampling_Program
          #+ Animal_Host
           # + Species:c
          )

##need those approximate decimals to be converted to exact values



