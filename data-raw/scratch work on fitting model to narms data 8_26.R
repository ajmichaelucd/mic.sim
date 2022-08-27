
rm(list = ls())
set.seed(4)
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
narms_SA <- full_narms %>% filter(Genus == "Salmonella")


not_all_na <- function(x) !all(is.na(x)|x==0|x=="<NA>")

narms_SA %>% select(where(not_all_na))

hist(log2(narms_SA$Amoxicillin_Clavulanic_Acid_MIC))
hist(log2(narms_SA$Ampicillin_MIC))
hist(log2(narms_SA$Azithromycin_MIC))
hist(log2(narms_SA$Cefoxitin_MIC))
hist(log2(narms_SA$Ceftiofur_MIC))
hist(log2(narms_SA$Ceftriaxone_MIC))
hist(log2(narms_SA$Chloramphenicol_MIC))
hist(log2(narms_SA$Ciprofloxacin_MIC))
hist(log2(narms_SA$Gentamicin_MIC))
hist(log2(narms_SA$Kanamycin_MIC))
hist(log2(narms_SA$Nalidixic_Acid_MIC))
hist(log2(narms_SA$Streptomycin_MIC))
hist(log2(narms_SA$Sulfisoxazole_MIC))
hist(log2(narms_SA$Tetracycline_MIC))
hist(log2(narms_SA$Trimethoprim_Sulfamethoxazole_MI))

narms_EC %>%
  ggplot() +
  geom_histogram(aes(x = log2(Azithromycin_MIC), fill = Animal_Host), binwidth = 1)

ec_cip_mics <- import_mics(narms_EC$Azithromycin_MIC, narms_EC$Azithromycin_Operator)

ec_cip <- narms_EC %>%
  select(EstID:Updated_Profile) %>%
  mutate(Species = tolower(Species)) %>%
  tibble(., ec_cip_mics) %>%
  mutate(t = lubridate::decimal_date( lubridate::dmy(Collection_Date)) - 2013)

ec_cip %>% group_by(code_column) %>% summarize(pct = 100 * n() / nrow(ec_cip))

prep_sim_data_for_em(ec_cip, covariate_names = c("Sampling_Program", "Animal_Host", "Species"), scale = "MIC") %>%
  fit_model(., formula = Surv(time = left_bound, time2 = right_bound, type = "interval2") ~ 0 + c +
              strata(c) + t:c
            #+ t:Species
            #+ Sampling_Program
            #+ Animal_Host
            # + Species
            ,ncomp=1)
