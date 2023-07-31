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
                                                                   "text", "text", "text", "text", "skip",
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
                                                                   "text", "text", "text", "text", "skip",
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

dublin_data <- read_excel("~/Downloads/Salmonella Dublin_Worksheet_MIC_Results_HF.xlsx",
                                                         col_types = c("numeric", "text", "text",
                                                                       "text", "text", "date", "date", "text",
                                                                       "text", "text", "text", "text", "text",
                                                                       "text", "text", "text", "text", "text",
                                                                       "text", "text", "text", "text", "text",
                                                                       "text", "text", "numeric", "text",
                                                                       "text", "numeric", "text", "text",
                                                                       "text", "text", "text", "text", "numeric"))

#1:18 metadata

col.from <- brd_data_mh %>% select(contains("MIC...")) %>% colnames
col.to <- brd_data_mh %>% select(-contains("MIC...")) %>% select(-contains("S/I/R")) %>% select(-(1:17)) %>% colnames
brd_mh <- brd_data_mh %>% select(-all_of(col.to)) %>% rename_at(vars(col.from), ~col.to) %>% select(-contains("S/I/R"))
brd_pm <- brd_data_pm %>% select(-all_of(col.to)) %>% rename_at(vars(col.from), ~col.to) %>% select(-contains("S/I/R"))

dublin <- dublin_data %>% janitor::clean_names() %>% select(-contains("atb_")) %>% rename(gentamycin_mic = gentamycin_mic_mic)
dnames <- dublin %>% select(ampicillin_mic:gentamycin_mic) %>% colnames

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
   # ggMarginal(a, x = t, y = mid, data = df, type = "histogram", margins = c("both"), yparams = list(binwidth = 1), xparams = list(bins = 16), groupFill = TRUE)




}

drugs <- brd_pm %>% select(AMPICI:TYLO) %>% colnames

pdf("~/Desktop/july_2023/brd_pm.pdf", width=11, height=8.5)
map(drugs, ~preview_column(.x, brd_pm, "Date of Isolation", "decimal"))
dev.off()

pdf("~/Desktop/july_2023/dublin.pdf", width=11, height=8.5)
map(dnames, ~preview_column(.x, dublin, "year", "year"))
dev.off()
#for tms, there is an observation with lb -4 rb -3 but everything else is ≤-3










