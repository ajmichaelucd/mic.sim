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





#1:18 metadata

col.from <- brd_data_mh %>% select(contains("MIC...")) %>% colnames
col.to <- brd_data_mh %>% select(-contains("MIC...")) %>% select(-contains("S/I/R")) %>% select(-(1:16)) %>% colnames
brd_mh <- brd_data_mh %>% select(-all_of(col.to)) %>% rename_at(vars(col.from), ~col.to) %>% select(-contains("S/I/R"))
brd_pm <- brd_data_pm %>% select(-all_of(col.to)) %>% rename_at(vars(col.from), ~col.to) %>% select(-contains("S/I/R"))

dublin_gn <- dublin_data_gn %>% janitor::clean_names() %>% select(-contains("atb_")) %>% rename(gentamycin_mic = gentamycin_mic_mic)
dublin_bopo <- dublin_data_bopo %>% janitor::clean_names() %>% select(-contains("atb_")) %>% rename(enrofloxacin_mic = enrofloxacin_l_mic)
dnames_gn <- dublin_gn %>% select(ampicillin_mic:gentamycin_mic) %>% colnames
dnames_bopo <- dublin_bopo %>% select(ampicillin_mic:tylosin_tartrate_mic) %>% colnames

#brd_data_mh %>% select(all_of(mic_cols))
#brd_data_mh %>% select(-contains("MIC...")) %>% select(-contains("S/I/R")) %>% select(-(1:18)) %>% colnames
#
#
#a <- colnames(brd_data_mh) %>% tibble %>% filter(!grepl("S/I/R", .))
#a %>% filter(!grepl("MIC...", .))
#mic_cols <- a %>% filter(grepl("MIC...", .)) %>% filter(!(. %in% c("MIC Date"))) %>% pull
# preview_column <- function(column, data){
#
# import_mics((data %>% select(all_of(column))) %>% pull(column)) %>% mutate(left_bound = log2(left_bound),
#                                                   right_bound = log2(right_bound)) %>%
#     mutate(
#       mid =
#         case_when(
#           left_bound == -Inf ~ right_bound - 0.5,
#           right_bound == Inf ~ left_bound + 0.5,
#           TRUE ~ (left_bound + right_bound) / 2
#         ),
#       cens =
#         case_when(
#           left_bound == -Inf ~ "lc",
#           right_bound == Inf ~ "rc",
#           TRUE ~ "int"
#         )) %>%
#     filter(!is.na(left_bound)) %>% ggplot() +
#     geom_bar(aes(x = mid, fill = cens)) +
#     ggtitle(column)
# }


preview_column("AMPICI", brd_pm)
preview_column("CEFTIF", brd_pm)
preview_column("CLINDA", brd_pm)
preview_column("DANOFL", brd_pm)
preview_column("ENROFL", brd_pm)
preview_column("FLORFE", brd_pm)
preview_column((brd_pm %>% select(AMPICI:TYLO) %>% colnames)[12], brd_pm)






#m <- map(brd_pm %>% select(AMPICI:TYLO) %>% colnames, ~preview_column(.x, brd_pm))

m <- `names<-`(m, brd_pm %>% select(AMPICI:TYLO) %>% colnames)

cowplot::plot_grid(plotlist = lapply(m, as.ggplot))



brd_mh %>% summarize(.by = c(`Specimen Source`),
                     n = n()) %>% View

##new plotting approach





preview_column <- function(column, data, date_col, date_type){


if(date_type == "decimal"){

  df_temp <- data %>% rename(date = date_col) %>%
    mutate(t = decimal_date(date) - 2007) %>% suppressWarnings()
} else if(date_type == "year"){
  df_temp <- data %>% rename(date = date_col) %>% rowwise %>%
    mutate(t = as.numeric(date) + runif(1, -0.35, 0.35)) %>% ungroup %>%
    suppressWarnings()
}else{
  errorCondition("pick decimal or year")
}

  import_mics((data %>% select(all_of(column))) %>% pull(column)) %>% mutate(left_bound = log2(left_bound),
                                                                             right_bound = log2(right_bound)) %>%
    mutate(
      cens =
        case_when(
          left_bound == -Inf ~ "lc",
          right_bound == Inf ~ "rc",
          TRUE ~ "int"
        ),
      mid =
        case_when(
          left_bound == -Inf ~ right_bound - 0.5,
          right_bound == Inf ~ left_bound + 0.5,
          TRUE ~ (left_bound + right_bound) / 2
        )) %>%
    tibble(., t = df_temp$t) %>%
    filter(!is.na(left_bound)) -> df

  if(nrow(df %>% filter(left_bound == -Inf)) > 0){
    plot_min <- (df %>% filter(left_bound == -Inf) %>% pull(right_bound) %>% min) - 1
  }else{
    plot_min <- (df %>% pull(left_bound) %>% min) - 1
  }

  if(nrow(df %>% filter(right_bound == Inf)) > 0){
    plot_max <- (df %>% filter(right_bound == Inf) %>% pull(left_bound) %>% max) + 1
  }else{
    plot_max <- (df %>% pull(right_bound) %>% max) + 1
  }


 #   df %>% ggplot() +
 #   #geom_bar(aes(x = mid, fill = cens)) +
 #   geom_segment(aes(x = t, xend = t, y = left_bound, yend = right_bound, color = cens)) +
 #     geom_point(aes(x = t, y = left_bound), data = df %>% filter(left_bound != -Inf)) +
 #     geom_point(aes(x = t, y = right_bound), data = df %>% filter(right_bound != Inf)) +
 #   ylim(plot_min, plot_max) +
 #   ggtitle(column) +
 #     xlab("Time") +
 #     ylab("MIC")


  df %>% ggplot() +
      #geom_bar(aes(x = mid, fill = cens)) +
      geom_point(aes(x = t, y = mid, color = cens), data = df, alpha = 0) +
      geom_segment(aes(x = t, xend = t, y = left_bound, yend = right_bound, color = cens), data = (df %>% filter(left_bound != -Inf & right_bound != Inf))) +
      geom_segment(aes(x = t, xend = t, y = right_bound, yend = left_bound, color = cens), data = (df %>% filter(left_bound == -Inf & right_bound != Inf) %>% mutate(left_bound = right_bound - 1.5)), arrow = arrow(length = unit(0.03, "npc"))) +
      geom_segment(aes(x = t, xend = t, y = left_bound, yend = right_bound, color = cens), data = (df %>% filter(left_bound != -Inf & right_bound == Inf) %>% mutate(right_bound = left_bound + 1.5)), arrow = arrow(length = unit(0.03, "npc"))) +
      geom_point(aes(x = t, y = left_bound), data = df %>% filter(left_bound != -Inf)) +
      geom_point(aes(x = t, y = right_bound), data = df %>% filter(right_bound != Inf)) +
      ylim(plot_min - 0.5, plot_max + 0.5) +
      ggtitle(column) +
      xlab("Time") +
      ylab("MIC")
    #ggMarginal(a, x = t, y = mid, data = df, type = "histogram", margins = c("both"), yparams = list(binwidth = 1), xparams = list(bins = 16), groupFill = TRUE)




}

drugs <- brd_pm %>% select(AMPICI:TYLO) %>% colnames

pdf("~/Desktop/july_2023/brd_pm.pdf", width=11, height=8.5)
map(drugs, ~preview_column(.x, brd_pm, "Date of Isolation", "decimal"))
dev.off()

pdf("~/Desktop/july_2023/dublin_bopo.pdf", width=11, height=8.5)
map(dnames_bopo, ~preview_column(.x, dublin_bopo, "year", "year"))
dev.off()
#for tms, there is an observation with lb -4 rb -3 but everything else is ≤-3



#"AMPICI"              "CEFTIF"              "CLINDA"
#"DANOFL"              "ENROFL"              "FLORFE"              "GAMITH"              "GENTAM"
#"NEOMYC"              "PENICI"              "SDIMET"              "SPECT"               "TETRA"
#"TIAMUL"              "TILDIP"              "TILMIC"              "TRISUL"              "TULATH"
#"TYLO"

brd_pm

drug = "GENTAM"

df = brd_pm %>%
  mutate(source = tolower(`Specimen Source`),
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
           ),
         t = decimal_date(`Date of Isolation`) - 2007)


#dublin_bopo %>% summarise(.by = year, n = n())

cens_dir = "LC"
ncomp = 2

#df <- dublin_bopo %>% mutate(
#  t = year - 1993
#)

#drug_name = "tulathromycin"
#drug = paste0(drug_name, "_mic")

import_mics(df %>% pull(drug)) %>% mutate(left_bound = log2(left_bound),
                                          right_bound = log2(right_bound)) %>%
  mutate(
    cens =
      case_when(
        left_bound == -Inf ~ "lc",
        right_bound == Inf ~ "rc",
        TRUE ~ "int"
      ),
    mid =
      case_when(
        left_bound == -Inf ~ right_bound - 0.5,
        right_bound == Inf ~ left_bound + 0.5,
        TRUE ~ (left_bound + right_bound) / 2
      )) %>%
  tibble(., t = df$t) -> df_temp

low_con <- case_when(
  nrow(df_temp %>% filter(left_bound == -Inf)) == 0 ~ min(df_temp$left_bound),
  TRUE ~ ifelse(nrow(df_temp %>% filter(left_bound == -Inf)) == 0, 0, df_temp %>% filter(left_bound == -Inf) %>% pull(right_bound) %>% unique))

high_con <- case_when(
  nrow(df_temp %>% filter(right_bound == Inf)) == 0 ~ max(df_temp$right_bound),
  TRUE ~ ifelse( nrow(df_temp %>% filter(right_bound == Inf)) == 0, 0, df_temp %>% filter(right_bound == Inf) %>% pull(left_bound) %>% unique))

df_temp %>% mutate(low_con = low_con, high_con = high_con) %>% mutate(obs_id = row_number()) %>% filter(!is.na(left_bound) & !is.na(right_bound)) -> visible_data

 output <- fit_model_pi(visible_data = visible_data,
               formula = Surv(time = left_bound,
                                                time2 = right_bound,
                                                type = "interval2") ~ pspline(t, df = 0, calc = TRUE),
               formula2 = c == "2" ~ s(t),
               max_it = 3000,
               ncomp = ncomp,
               tol_ll = 1e-06,
               pi_link = "logit",
               verbose = 3,
               initial_weighting = 8)






 df = output$possible_data %>% mutate(cens =
                                        case_when(
                                          left_bound == -Inf ~ "lc",
                                          right_bound == Inf ~ "rc",
                                          TRUE ~ "int"
                                        ),
                                      mid =
                                        case_when(
                                          left_bound == -Inf ~ right_bound - 0.5,
                                          right_bound == Inf ~ left_bound + 0.5,
                                          TRUE ~ (left_bound + right_bound) / 2
                                        ))

 if(nrow(df %>% filter(left_bound == -Inf)) > 0){
   plot_min <- (df %>% filter(left_bound == -Inf) %>% pull(right_bound) %>% min) - 1
 }else{
   plot_min <- (df %>% pull(left_bound) %>% min) - 1
 }

 if(nrow(df %>% filter(right_bound == Inf)) > 0){
   plot_max <- (df %>% filter(right_bound == Inf) %>% pull(left_bound) %>% max) + 1
 }else{
   plot_max <- (df %>% pull(right_bound) %>% max) + 1
 }

 #ciTools::add_pi(df, output$newmodel[[1]], alpha = 0.05, names = c("lwr", "upr"))
    #doesn't work with gaussian dist


 mu.se.brd <- function(t, c, z){predict(output$newmodel[[c]], data.frame(t = t)) + z * predict(output$newmodel[[c]], data.frame(t = t), se = TRUE)$se.fit}
 mu.se.brd.fms <- function(t, z){predict(output$newmodel, data.frame(t = t)) + z * predict(output$newmodel, data.frame(t = t), se = TRUE)$se.fit}

 if(ncomp == 2){

   output$newmodel[[1]]$scale %>% print
   output$newmodel[[2]]$scale %>% print

  ci_data <- tibble(t = rep(seq(0, max(output$possible_data$t), len = 300), 2)) %>%
     mutate(
       c1pred = predict(output$newmodel[[1]], tibble(t), se = T)$fit,
       c1pred_se = predict(output$newmodel[[1]], tibble(t), se = T)$se.fit,
       c1pred_lb = c1pred - 1.96 * c1pred_se,
       c1pred_ub = c1pred + 1.96 * c1pred_se,
       c2pred = predict(output$newmodel[[2]], tibble(t), se = T)$fit,
       c2pred_se = predict(output$newmodel[[2]], tibble(t), se = T)$se.fit,
       c2pred_lb = c2pred - 1.96 * c2pred_se,
       c2pred_ub = c2pred + 1.96 * c2pred_se,
     )

 df %>% ggplot() +
   #geom_bar(aes(x = mid, fill = cens)) +
   geom_point(aes(x = t, y = mid, color = `P(C=c|y,t)`), data = df %>% filter(c == "2"), alpha = 0) +
   geom_segment(aes(x = t, xend = t, y = left_bound, yend = right_bound, color = `P(C=c|y,t)`), data = (df %>% filter(cens == "int" & c == "2")), alpha = 0.3) +
   geom_segment(aes(x = t, xend = t, y = right_bound, yend = left_bound, color = `P(C=c|y,t)`), data = (df %>% filter(cens == "lc" & c == "2") %>% mutate(left_bound = right_bound - 1.5)), arrow = arrow(length = unit(0.03, "npc")), alpha = 0.3) +
   geom_segment(aes(x = t, xend = t, y = left_bound, yend = right_bound, color = `P(C=c|y,t)`), data = (df %>% filter(cens == "rc" & c == "2") %>% mutate(right_bound = left_bound + 1.5)), arrow = arrow(length = unit(0.03, "npc")), alpha = 0.3) +
   geom_point(aes(x = t, y = left_bound,  color = `P(C=c|y,t)`), data = df %>% filter(left_bound != -Inf & c == "2"), alpha = 0.3) +
   geom_point(aes(x = t, y = right_bound,  color = `P(C=c|y,t)`), data = df %>% filter(right_bound != Inf & c == "2"), alpha = 0.3) +
   scale_colour_gradientn(colours = c("purple", "darkorange")) +
   #ylim(plot_min - 0.5, plot_max + 0.5) +
   ggtitle(drug) +
   xlab("Time") +
   ylab("MIC") +
   ggnewscale::new_scale_color() +
   geom_function(fun = function(t){predict(output$newmodel[[1]], newdata = data.frame(t = t))}, aes(color = "Component 1 Mu", linetype = "Fitted Model")) +
   geom_function(fun = function(t){predict(output$newmodel[[2]], newdata = data.frame(t = t))}, aes(color = "Component 2 Mu", linetype = "Fitted Model")) +
   #geom_function(fun = function(t){mu.se.brd(t, c = 1, z = 1.96)}, aes(color = "Component 1 Mu", linetype = "Fitted Model SE"), size = 0.6, alpha = 0.6) +
   #geom_function(fun = function(t){mu.se.brd(t, c = 1, z = -1.96)}, aes(color = "Component 1 Mu", linetype = "Fitted Model SE"), size = 0.6, alpha = 0.6) +
   #geom_function(fun = function(t){mu.se.brd(t, c = 2, z = 1.96)}, aes(color = "Component 2 Mu", linetype = "Fitted Model SE"), size = 0.6, alpha = 0.6) +
   #geom_function(fun = function(t){mu.se.brd(t, c = 2, z = -1.96)}, aes(color = "Component 2 Mu", linetype = "Fitted Model SE"), size = 0.6, alpha = 0.6) +
   geom_ribbon(aes(ymin = c1pred_lb, ymax = c1pred_ub, x = t, fill = "Component 1 Mu"), data = ci_data, alpha = 0.25) +
   geom_ribbon(aes(ymin = c2pred_lb, ymax = c2pred_ub, x = t, fill = "Component 2 Mu"), data = ci_data, alpha = 0.25) +
   geom_ribbon(aes(ymin = lwr, ymax = upr, x = t, fill = "Component 1 Mu"), data = sim_pi_survreg_boot(df, fit = output$newmodel[[1]], alpha = 0.05, nSims = 10000), alpha = 0.15) +
   geom_ribbon(aes(ymin = lwr, ymax = upr, x = t, fill = "Component 2 Mu"), data = sim_pi_survreg_boot(df, fit = output$newmodel[[2]], alpha = 0.05, nSims = 10000), alpha = 0.15)

# need to examine the things for sim_pi_survreg_boot, specifically the vcov stuff and if we should let it draw values for all spline terms and then also for the
 #way it calculates the sim response
  #do we need to account for weighting or anything?

 }else{

   ci_data <- tibble(t = rep(seq(0, max(output$possible_data$t), len = 300), 2)) %>%
     mutate(
       c1pred = predict(output$newmodel, tibble(t), se = T)$fit,
       c1pred_se = predict(output$newmodel, tibble(t), se = T)$se.fit,
       c1pred_lb = c1pred - 1.96 * c1pred_se,
       c1pred_ub = c1pred + 1.96 * c1pred_se
     )

   output$newmodel$scale %>% print()

 df %>% ggplot() +
   #geom_bar(aes(x = mid, fill = cens)) +
   #geom_point(aes(x = t, y = mid, color = `P(C=c|y,t)`), data = df %>% filter(c == "2"), alpha = 0) +
   geom_segment(aes(x = t, xend = t, y = left_bound, yend = right_bound, color = cens), data = (df %>% filter(cens == "int")), alpha = 0.2) +
   geom_segment(aes(x = t, xend = t, y = right_bound, yend = left_bound, color = cens), data = (df %>% filter(cens == "lc") %>% mutate(left_bound = right_bound - 1.5)), arrow = arrow(length = unit(0.03, "npc")), alpha = 0.2) +
   geom_segment(aes(x = t, xend = t, y = left_bound, yend = right_bound, color = cens), data = (df %>% filter(cens == "rc") %>% mutate(right_bound = left_bound + 1.5)), arrow = arrow(length = unit(0.03, "npc")), alpha = 0.2) +
   geom_point(aes(x = t, y = left_bound,  color = cens), data = df %>% filter(left_bound != -Inf), alpha = 0.2) +
   geom_point(aes(x = t, y = right_bound,  color = cens), data = df %>% filter(right_bound != Inf), alpha = 0.2) +
   #scale_colour_gradientn(colours = c("purple", "orange")) +
   #ylim(plot_min - 0.5, plot_max + 0.5) +
   ggtitle(drug) +
   xlab("Time") +
   ylab("MIC") +
   ggnewscale::new_scale_color() +
   geom_function(fun = function(t){predict(output$newmodel, newdata = data.frame(t = t))}, aes(color = "Component Mu", linetype = "Fitted Model")) +
   #geom_function(fun = function(t){predict(output$newmodel[[2]], newdata = data.frame(t = t))}, aes(color = "Component 2 Mu", linetype = "Fitted Model")) +
   #geom_function(fun = function(t){mu.se.brd(t, c = 1, z = 1.96)}, aes(color = "Component 1 Mu", linetype = "Fitted Model SE"), size = 0.6, alpha = 0.6) +
   #geom_function(fun = function(t){mu.se.brd(t, c = 1, z = -1.96)}, aes(color = "Component 1 Mu", linetype = "Fitted Model SE"), size = 0.6, alpha = 0.6) +
   geom_function(fun = function(t){mu.se.brd.fms(t, z = 1.96)}, aes(color = "Component Mu", linetype = "Fitted Model SE"), size = 0.6, alpha = 0.6) +
   geom_function(fun = function(t){mu.se.brd.fms(t, z = -1.96)}, aes(color = "Component Mu", linetype = "Fitted Model SE"), size = 0.6, alpha = 0.6) +
   geom_ribbon(aes(ymin = c1pred_lb, ymax = c1pred_ub, x = t, fill = "Component 1 Mu"), data = ci_data, alpha = 0.2)




}


##maxing out iterations fails to generate a `converge` object!!!!!!!!!!!!!!


 ggplot() +
   geom_function(fun = function(t){predict(output$binom_model, newdata = data.frame(t = t), type = "response")}) +
   xlim(0, 16) +
   ylim(0,1)



 output <- fit_model_safety_pi(visible_data = visible_data,
                               formula = Surv(time = left_bound, time2 = right_bound, type = "interval2") ~ pspline(t, df = 0, calc = TRUE),
                               formula2 = c == "2" ~ s(t),
                               fm_check = cens_dir,
                               max_it = 3000,
                               ncomp = 2,
                               tol_ll = 1e-06,
                               browse_at_end = FALSE,
                               browse_each_step = FALSE,
                               plot_visuals = FALSE,
                               pi_link = "logit",
                               verbose = 3,
                               maxiter_survreg = 30
 )






 df = output$possible_data %>% mutate(cens =
                                        case_when(
                                          left_bound == -Inf ~ "lc",
                                          right_bound == Inf ~ "rc",
                                          TRUE ~ "int"
                                        ),
                                      mid =
                                        case_when(
                                          left_bound == -Inf ~ right_bound - 0.5,
                                          right_bound == Inf ~ left_bound + 0.5,
                                          TRUE ~ (left_bound + right_bound) / 2
                                        ))

 if(nrow(df %>% filter(left_bound == -Inf)) > 0){
   plot_min <- (df %>% filter(left_bound == -Inf) %>% pull(right_bound) %>% min) - 1
 }else{
   plot_min <- (df %>% pull(left_bound) %>% min) - 1
 }

 if(nrow(df %>% filter(right_bound == Inf)) > 0){
   plot_max <- (df %>% filter(right_bound == Inf) %>% pull(left_bound) %>% max) + 1
 }else{
   plot_max <- (df %>% pull(right_bound) %>% max) + 1
 }

 mu.se.brd.safety <- function(t, z){predict(output$newmodel, data.frame(t = t)) + z * predict(output$newmodel, data.frame(t = t), se = TRUE)$se.fit}

 means <- df %>% ggplot() +
   #geom_bar(aes(x = mid, fill = cens)) +
   geom_point(aes(x = t, y = mid, color = `P(C=c|y,t)`), data = df %>% filter(c == "2"), alpha = 0) +
   geom_segment(aes(x = t, xend = t, y = left_bound, yend = right_bound, color = `P(C=c|y,t)`), data = (df %>% filter(cens == "int" & c == "2")), alpha = 0.2) +
   geom_segment(aes(x = t, xend = t, y = right_bound, yend = left_bound, color = `P(C=c|y,t)`), data = (df %>% filter(cens == "lc" & c == "2") %>% mutate(left_bound = right_bound - 1.5)), arrow = arrow(length = unit(0.03, "npc")), alpha = 0.2) +
   geom_segment(aes(x = t, xend = t, y = left_bound, yend = right_bound, color = `P(C=c|y,t)`), data = (df %>% filter(cens == "rc" & c == "2") %>% mutate(right_bound = left_bound + 1.5)), arrow = arrow(length = unit(0.03, "npc")), alpha = 0.2) +
   geom_point(aes(x = t, y = left_bound, color = `P(C=c|y,t)`), data = df %>% filter(left_bound != -Inf), alpha = 0.2) +
   geom_point(aes(x = t, y = right_bound, color = `P(C=c|y,t)`), data = df %>% filter(right_bound != Inf), alpha = 0.2) +
   scale_colour_gradientn(colours = c("purple", "orange")) +
   #ylim(plot_min - 0.5, plot_max + 0.5) +
   ggtitle(drug) +
   xlab("Time") +
   ylab("MIC") +
   ggnewscale::new_scale_color() +
   geom_function(fun = function(t){predict(output$newmodel, newdata = data.frame(t = t))}, aes(color = "Component 1 Mu", linetype = "Fitted Model")) +
 #  geom_function(fun = function(t){predict(output$newmodel[[2]], newdata = data.frame(t = t))}, aes(color = "Component 2 Mu", linetype = "Fitted Model")) +
   geom_function(fun = function(t){mu.se.brd.safety(t, z = 1.96)}, aes(color = "Component 1 Mu", linetype = "Fitted Model SE"), size = 0.6, alpha = 0.6) +
   geom_function(fun = function(t){mu.se.brd.safety(t, z = -1.96)}, aes(color = "Component 1 Mu", linetype = "Fitted Model SE"), size = 0.6, alpha = 0.6)
 #  geom_function(fun = function(t){mu.se.brd(t, c = 2, z = 1.96)}, aes(color = "Component 2 Mu", linetype = "Fitted Model SE"), size = 0.6, alpha = 0.6) +
 #  geom_function(fun = function(t){mu.se.brd(t, c = 2, z = -1.96)}, aes(color = "Component 2 Mu", linetype = "Fitted Model SE"), size = 0.6, alpha = 0.6)


pi <- df %>%
     ggplot() +
     #  geom_point(aes(x = t, y = pi_hat, color = "Fitted Model")) +
     geom_function(
       fun = function(t) {
         predict(output$binom_model, data.frame(t), type = "response")
       },
       aes(color = "Proportion non-WT")
     ) +
  xlim(min(output$possible_data$t), max(output$possible_data$t)) +
  ylim(0, 1)

means/pi








dublin_bopo %>% summarise(.by = gentamicin_mic, n = n())
23
247










