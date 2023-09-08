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


prep_df("mh", "GENTAM", brd_mh) -> df_temp

low_con <- case_when(
  nrow(df_temp %>% filter(left_bound == -Inf)) == 0 ~ min(df_temp$left_bound),
  TRUE ~ ifelse(nrow(df_temp %>% filter(left_bound == -Inf)) == 0, 0, df_temp %>% filter(left_bound == -Inf) %>% pull(right_bound) %>% unique))

high_con <- case_when(
  nrow(df_temp %>% filter(right_bound == Inf)) == 0 ~ max(df_temp$right_bound),
  TRUE ~ ifelse( nrow(df_temp %>% filter(right_bound == Inf)) == 0, 0, df_temp %>% filter(right_bound == Inf) %>% pull(left_bound) %>% unique))

visible_data = df_temp %>%
  mutate(low_con = low_con, high_con = high_con) %>%
  mutate(obs_id = row_number()) %>%
  filter(!is.na(left_bound) & !is.na(right_bound))


single_model_output_fm <- visible_data %>%
  fit_model_pi(visible_data = .,
                       formula = Surv(time = left_bound,
                                      time2 = right_bound,
                                      type = "interval2") ~ pspline(t, df = 0, calc = TRUE),
                       formula2 = c == "2" ~ s(t),
                       max_it = 1,
                       ncomp = 2,
                       tol_ll = 1e-06,
                       pi_link = "logit",
                       verbose = 3,
                       initial_weighting = 8)
if (length(single_model_output_fm) == 1) {
  fm_convergence = FALSE
  print("did not converge")
}else{
  fm_convergence = case_when(single_model_output_fm$converge %in% c("YES", "iterations") & ncomp > 1 ~ TRUE,
                             single_model_output_fm$ncomp == 1 ~ TRUE,
                             TRUE ~ FALSE)
  print("model converged, starting model checks")
  #model converged, so let's check the fit
}


if(fm_convergence & single_model_output_fm$ncomp > 1){
  max_scale <-
    max(single_model_output_fm$newmodel[[1]]$scale,
        single_model_output_fm$newmodel[[2]]$scale)
  max_difference_mu_hat <-
    max(abs(
      predict(single_model_output_fm$newmodel[[2]], newdata = visible_data) - predict(single_model_output_fm$newmodel[[1]], newdata = visible_data)
    ))
  if (max_scale ^ 2 > max_difference_mu_hat) {
    sigma_check = "stop"
    #censor_fm_check = NA_character_
    #fms_convergence = "tbd"
    if (verbose > 1) {print(paste0("sigma check results: ", sigma_check))}
  } else{

    sigma_check = "go"
    if (verbose > 1) {print(paste0("sigma check results: ", sigma_check))}
  }

  censor_fm_check <-
    check_for_excessive_censoring(single_model_output_fm, cutoff)

  if (verbose > 1) {print(paste0("censoring check results: ", censor_fm_check))}

  if (censor_fm_check == "BOTH") {
    fms_convergence = NA_character_
    single_model_output_fms = "PASS"
    if (verbose > 1) {
      print(
        "fit_model converged outside excessive censoring boundaries in both directions"
      )
    }
  } else if (censor_fm_check == "All Clear" & sigma_check == "go") {
    fms_convergence = NA_character_
    single_model_output_fms = "PASS"
    if (verbose > 1) {
      print("fit_model converged within excessive censoring boundaries")
    }
  } else if (censor_fm_check == "All Clear" & sigma_check == "stop") {
    fms_convergence = NA_character_
    single_model_output_fms = "PASS"
    if (verbose > 1) {
      print("fit_model converged within excessive censoring boundaries but violated sigma check, cannot find a side to use for fms")
    }
  } else{
    fms_convergence = "tbd"
  }
}
if(length(single_model_output_fm) > 1 & !fm_convergence & single_model_output_fm$ncomp > 1){ ##put the stuff for a model that didn't converge but generated output here, assign censor_fm_check as "RC", "LC", or both? skip sigma check

  sigma_check = NA_character_ #can't sigma check because one scale is missing
  censor_fm_check =
    case_when(
      is.na(single_model_output_fm$newmodel[[1]]$coefficients[1]) & is.na(single_model_output_fm$newmodel[[2]]$coefficients[1]) ~ "BOTH",
      is.na(single_model_output_fm$newmodel[[1]]$coefficients[1]) ~ "LC",
      is.na(single_model_output_fm$newmodel[[2]]$coefficients[1]) ~ "RC",
      TRUE ~ "BOTH"
    )
  fms_convergence = case_when(
    censor_fm_check == "BOTH" ~ NA,
    TRUE ~ "tbd"
  )
  if(is.na(fms_convergence)){single_model_output_fms = "PASS"}

}
if(length(single_model_output_fm) == 1 | single_model_output_fm$ncomp == 1){
  sigma_check = NA_character_
  censor_fm_check = NA_character_
  fms_convergence = "PASS"
}

plot_fm <- function(output, title){

  ncomp = single_model_output_fm$ncomp

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
  plot_min_1 <- (df %>% filter(left_bound == -Inf) %>% pull(right_bound) %>% min) - 1
}else{
  plot_min_1 <- (df %>% pull(left_bound) %>% min) - 1
}


if(ncomp == 1){
  plot_min_2 <- sim_pi_survreg_boot(df, fit = output$newmodel, alpha = 0.05, nSims = 10000) %>% pull(lwr) %>% min - 0.2
} else if(ncomp == 2){
  plot_min_2 <- min(sim_pi_survreg_boot(df, fit = output$newmodel[[1]], alpha = 0.05, nSims = 10000) %>% pull(lwr) %>% min - 0.2,
                    sim_pi_survreg_boot(df, fit = output$newmodel[[2]], alpha = 0.05, nSims = 10000) %>% pull(lwr) %>% min - 0.2)
}else{
  plot_min_2 = plot_min_1
}

plot_min = min(plot_min_1, plot_min_2)


if(nrow(df %>% filter(right_bound == Inf)) > 0){
  plot_max_1 <- (df %>% filter(right_bound == Inf) %>% pull(left_bound) %>% max) + 1
}else{
  plot_max_1 <- (df %>% pull(right_bound) %>% max) + 1
}

if(ncomp == 1){
  plot_max_2 <- sim_pi_survreg_boot(df, fit = output$newmodel, alpha = 0.05, nSims = 10000) %>% pull(upr) %>% max + 0.2
} else if(ncomp == 2){
  plot_max_2 <- max(sim_pi_survreg_boot(df, fit = output$newmodel[[1]], alpha = 0.05, nSims = 10000) %>% pull(upr) %>% max + 0.2,
                    sim_pi_survreg_boot(df, fit = output$newmodel[[2]], alpha = 0.05, nSims = 10000) %>% pull(upr) %>% max + 0.2)
}else{
  plot_max_2 = plot_max_1
}

plot_max = max(plot_max_1, plot_max_2)


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

  mean <- df %>% ggplot() +
    #geom_bar(aes(x = mid, fill = cens)) +
    geom_point(aes(x = t, y = mid, color = `P(C=c|y,t)`), data = df %>% filter(c == "2"), alpha = 0) +
    geom_segment(aes(x = t, xend = t, y = left_bound, yend = right_bound, color = `P(C=c|y,t)`), data = (df %>% filter(cens == "int" & c == "2")), alpha = 0.3) +
    geom_segment(aes(x = t, xend = t, y = right_bound, yend = left_bound, color = `P(C=c|y,t)`), data = (df %>% filter(cens == "lc" & c == "2") %>% mutate(plot_min)), arrow = arrow(length = unit(0.03, "npc")), alpha = 0.3) +
    geom_segment(aes(x = t, xend = t, y = left_bound, yend = right_bound, color = `P(C=c|y,t)`), data = (df %>% filter(cens == "rc" & c == "2") %>% mutate(plot_max)), arrow = arrow(length = unit(0.03, "npc")), alpha = 0.3) +
    geom_point(aes(x = t, y = left_bound,  color = `P(C=c|y,t)`), data = df %>% filter(left_bound != -Inf & c == "2"), alpha = 0.3) +
    geom_point(aes(x = t, y = right_bound,  color = `P(C=c|y,t)`), data = df %>% filter(right_bound != Inf & c == "2"), alpha = 0.3) +
    scale_colour_gradientn(colours = c("purple", "darkorange")) +
    #ylim(plot_min - 0.5, plot_max + 0.5) +
    ggtitle(title) +
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
    geom_ribbon(aes(ymin = lwr, ymax = upr, x = t, fill = "Component 2 Mu"), data = sim_pi_survreg_boot(df, fit = output$newmodel[[2]], alpha = 0.05, nSims = 10000), alpha = 0.15) +
    ylim(plot_min, plot_max)

  ##find sim_pi_survreg_boot in scratch_add_pi_survreg.R

  # need to examine the things for sim_pi_survreg_boot, specifically the vcov stuff and if we should let it draw values for all spline terms and then also for the
  #way it calculates the sim response
  #do we need to account for weighting or anything?

  pi <- ggplot() +
    geom_function(fun = function(t){predict(output$binom_model, newdata = data.frame(t = t), type = "response")}) +
    xlim(0, 16) +
    ylim(0,1)

  mean/pi

}else{

  ci_data <- tibble(t = rep(seq(0, max(output$possible_data$t), len = 300), 2)) %>%
    mutate(
      c1pred = predict(output$newmodel, tibble(t), se = T)$fit,
      c1pred_se = predict(output$newmodel, tibble(t), se = T)$se.fit,
      c1pred_lb = c1pred - 1.96 * c1pred_se,
      c1pred_ub = c1pred + 1.96 * c1pred_se
    )

  output$newmodel$scale %>% print()

  mean <- df %>% ggplot() +
    #geom_bar(aes(x = mid, fill = cens)) +
    #geom_point(aes(x = t, y = mid, color = `P(C=c|y,t)`), data = df %>% filter(c == "2"), alpha = 0) +
    geom_segment(aes(x = t, xend = t, y = left_bound, yend = right_bound, color = cens), data = (df %>% filter(cens == "int")), alpha = 0.2) +
    geom_segment(aes(x = t, xend = t, y = right_bound, yend = left_bound, color = cens), data = (df %>% filter(cens == "lc") %>% mutate(left_bound = plot_min)), arrow = arrow(length = unit(0.03, "npc")), alpha = 0.2) +
    geom_segment(aes(x = t, xend = t, y = left_bound, yend = right_bound, color = cens), data = (df %>% filter(cens == "rc") %>% mutate(right_bound = plot_max)), arrow = arrow(length = unit(0.03, "npc")), alpha = 0.2) +
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
    geom_ribbon(aes(ymin = c1pred_lb, ymax = c1pred_ub, x = t, fill = "Component 1 Mu"), data = ci_data, alpha = 0.2) +
    geom_ribbon(aes(ymin = lwr, ymax = upr, x = t, fill = "Component 1 Mu"), data = sim_pi_survreg_boot(df, fit = output$newmodel, alpha = 0.05, nSims = 10000), alpha = 0.15) +
    ylim(plot_min, plot_max)


mean

}


##maxing out iterations fails to generate a `converge` object!!!!!!!!!!!!!!

}
plot_fm(single_model_output_fm, "GENTAM-MH")
single_model_output




