
rm(list = ls())
set.seed(4)
library(devtools)
library(magrittr)
library(dplyr)
#<<<<<<< HEAD
load_all()


#NEXT STEPS:
##DIFFERENT SIGMAS
##INTERVAL CENSORING
##MORE THAN 2 COMPONENTS
##FUNCTIONS OF TIME AND COVARIATES: MEANS AND PI

#=======
library(mic.sim)
library(ggplot2)
library(LearnBayes)
library(survival)
library(biglm)
library(gridExtra)
library(readxl)
full_narms <- read.csv("~/Desktop/2021-FSIS-00065-F_3663/FOIA2021-065.csv", na.strings="NULL")
narms_EC <- full_narms %>% filter(Genus == "E. coli")


#not_all_na <- function(x) !all(is.na(x)|x==0|x=="<NA>")
#
#narms_SA %>% select(where(not_all_na))
#
#hist(log2(narms_SA$Amoxicillin_Clavulanic_Acid_MIC))
#hist(log2(narms_SA$Ampicillin_MIC))
#hist(log2(narms_SA$Azithromycin_MIC))
#hist(log2(narms_SA$Cefoxitin_MIC))
#hist(log2(narms_SA$Ceftiofur_MIC))
#hist(log2(narms_SA$Ceftriaxone_MIC))
#hist(log2(narms_SA$Chloramphenicol_MIC))
#hist(log2(narms_SA$Ciprofloxacin_MIC))
#hist(log2(narms_SA$Gentamicin_MIC))
#hist(log2(narms_SA$Kanamycin_MIC))
#hist(log2(narms_SA$Nalidixic_Acid_MIC))
#hist(log2(narms_SA$Streptomycin_MIC))
#hist(log2(narms_SA$Sulfisoxazole_MIC))
#hist(log2(narms_SA$Tetracycline_MIC))
#hist(log2(narms_SA$Trimethoprim_Sulfamethoxazole_MI))
#
narms_EC %>%
  ggplot() +
  geom_freqpoly(aes(x = log2(Ampicillin_MIC), color = Animal_Host, after_stat(density)), binwidth = 1, ) +
  facet_wrap(~Animal_Host)

ec_amp_mics <- import_mics(narms_EC$Ampicillin_MIC, narms_EC$Ampicillin_Operator)

ec_amp <- narms_EC %>%
  select(EstID:Updated_Profile) %>%
  mutate(Species = tolower(Species)) %>%
  tibble(., ec_amp_mics) %>%
  mutate(t = lubridate::decimal_date( lubridate::dmy(Collection_Date)) - 2013)

ec_amp %>% group_by(code_column) %>% summarize(pct = 100 * n() / nrow(ec_amp))

output <- prep_sim_data_for_em(ec_amp, covariate_names = c("Sampling_Program", "Animal_Host", "Species"), scale = "MIC") %>%
  fit_model(., formula = Surv(time = left_bound, time2 = right_bound, type = "interval2") ~ 0 + c +
              strata(c) + t:c
            #+ t:Species
            #+ Sampling_Program
            #+ Animal_Host
            # + Species
            ,ncomp=2, browse_each_step = TRUE)
