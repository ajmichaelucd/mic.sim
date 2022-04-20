#libraries------------
library(tidyverse)
library(magrittr)
library(mixtools)
library(data.table)


#select index-------------
index <- "test12722_A"



#import data---------
activefile <- paste(paste("run_results_", index, sep = ""), ".xlsx", sep = "")

sheets_to_grab <- tibble(names = c("data_centers", "data_censoring", "aft_results", "lr_results", "spaft_results",  "coef_data", "presets", "censored_df"), numbers = 1:8)

extract_sheets_specific <- function(filename, names, numbers){
  assign(names, readxl::read_xlsx(filename, sheet = numbers))
}

workbook <- map2(sheets_to_grab$names, sheets_to_grab$numbers, ~extract_sheets_specific(activefile, .x, .y))
names(workbook) <- c("data_centers", "data_censoring", "aft_results", "lr_results", "spaft_results", "coef_data", "presets", "censored_df")


#Preliminary work with parameters
median_matrix <- matrix(nrow = 100, ncol = nrow(workbook$coef_data))
for( j in 1:100){
  for ( i in 1:nrow(workbook$coef_data)) {
    median_matrix[j, i]  <- median(rnormmix(10000000, lambda = c(workbook$coef_data$lower_lambda[i], workbook$coef_data$upper_lambda[i]), mu = c(workbook$coef_data$lower_means[i], workbook$coef_data$upper_means[i]), sigma = c(workbook$coef_data$lsd[i], workbook$coef_data$usd[i]) ) )
  }
}
median_tibbles <- as_tibble(median_matrix, .name_repair = NULL )



value_parameters <- workbook$coef_data %>% 
  mutate(overall_mean = (lower_means * lower_lambda) + (upper_means * upper_lambda) ) %>% 
  mutate(overall_median = map_dbl(median_tibbles, mean)) %>% 
  mutate(year = row_number() - 1)









#Check out data centers--------------
centers_info <- workbook$data_centers
#avg/median mean/median for each year
centers_info %>% 
  group_by(year) %>% 
  summarise(avg_mean = mean(mean),
            median_mean = median(mean),
            avg_median = mean(median),
            median_median = median(median))
#boxplots of means for each year
centers_info %>% 
  ggplot() +
  geom_boxplot(aes(x = year - 0.1, y = mean, group = year), color = "orange") +
  geom_boxplot(aes(x = year + 0.1, y = median, group = year), color = "darkgreen") +
  geom_point(data = value_parameters, aes(x = year, y = overall_mean), color = "darkblue", shape = 5) +
  geom_point(data = value_parameters, aes(x = year, y = overall_median), color = "darkviolet", shape = 5)




#differences of means
wide_means_info <- centers_info %>% 
  pivot_wider(id_cols = id_arg, names_from = year, values_from = mean, names_prefix = "year_")  #make each year a column with id_arg as the index for each row/iteration: 1 row per iteration

temp_matrix <- matrix(nrow = nrow(wide_means_info), ncol = (ncol(wide_means_info) - 2))

for(n in 3:ncol(wide_means_info)){
  temp_matrix[, (n-2)] <- (as.matrix(wide_means_info)[ , n] - as.matrix(wide_means_info)[ , (n-1)])
}
differences_means_df <- as_tibble(temp_matrix, .name_repair = NULL)


create_diff_names_seq <- function(df, vec){
  paste(paste(paste("y", vec, sep = ""), "toy", sep = ""),  vec - 1, sep  = "")
}

names(differences_means_df) <- map2_chr(differences_means_df, c(1:4), ~create_diff_names_seq(.x, .y))

summary(differences_means_df)

differences_means_df %>%
  mutate(id_arg = row_number()) %>% 
  pivot_longer(cols = c(y1toy0, y2toy1, y3toy2, y4toy3)) %>% 
  ggplot() +
  geom_boxplot(aes( x = name, y = value))


#differences of medians
wide_medians_info <- centers_info %>% 
  pivot_wider(id_cols = id_arg, names_from = year, values_from = median, names_prefix = "year_")  #make each year a column with id_arg as the index for each row/iteration: 1 row per iteration

temp_matrix <- matrix(nrow = nrow(wide_medians_info), ncol = (ncol(wide_medians_info) - 2))

for(n in 3:ncol(wide_medians_info)){
  temp_matrix[, (n-2)] <- (as.matrix(wide_medians_info)[ , n] - as.matrix(wide_medians_info)[ , (n-1)])
}
differences_medians_df <- as_tibble(temp_matrix, .name_repair = NULL) 

names(differences_medians_df) <- map2_chr(differences_medians_df, c(1:4), ~create_diff_names_seq(.x, .y))

#Check out data censoring------------
data_censoring_active <- workbook$data_censoring
censoring_info <- data_censoring_active %>% 
  relocate(id_arg, .before = year) %>% 
  rename(left = X2, interval = X3, right = X0) %>% 
  replace_na( list(left = 0, interval = 0, right = 0))

censoring_info %>% 
  group_by(year) %>% 
  summarise(total_left = sum(left, na.rm = TRUE), total_interval = sum(interval, na.rm = TRUE), total_right = sum(right, na.rm = TRUE),
            prop_left = total_left / sum(total_left, total_interval, total_right), 
            prop_interval = total_interval / sum(total_left, total_interval, total_right),
            prop_right = total_right / sum(total_left, total_interval, total_right),
            mean_left = mean(left, na.rm = TRUE),
            mean_interval = mean(interval, na.rm = TRUE),
            mean_right = mean(right, na.rm = TRUE),
            median_left = median(left, na.rm = TRUE),
            median_interval = median(interval, na.rm = TRUE),
            median_right = median(right, na.rm = TRUE))

grab_iteration_cens <- function(number,df){
  df %>% 
    filter(id_arg == number)
}

grab_iteration_cens(400, censoring_info)


#grab some info from those AFTs-----------------


workbook$aft_results %>% 
  group_by(distribution) %>%
  summarise(mean_int_coef = mean(intercept_coef),
            mean_year_coef = mean(year_coef),
            mean_year_sq_coef = mean(year_sq_coef)) #maybe from here flip rows and columns and then map over these to calculate annual changes

workbook$aft_results %>% 
  ggplot() +
  geom_boxplot(aes(x = distribution, y = intercept_coef, group = distribution))

workbook$aft_results %>% 
  ggplot() +
  geom_boxplot(aes(x = distribution, y = year_coef, group = distribution))

workbook$aft_results %>% 
  ggplot() +
  geom_boxplot(aes(x = distribution, y = year_sq_coef, group = distribution))


AFT_CIs<- workbook$aft_results %>% 
  select(-c(ll_int, ll_full)) %>% 
  mutate(int_ci_lb = intercept_coef - 1.96 * std_error_int, 
         int_ci_ub = intercept_coef + 1.96 * std_error_int,
         year_ci_lb = year_coef - 1.96 * std_error_year,
         year_ci_ub = year_coef + 1.96 * std_error_year,
         year_sq_ci_lb = year_sq_coef - 1.96 * std_error_year_sq,
         year_sq_ci_ub = year_sq_coef + 1.96 * std_error_year_sq) %>% 
  mutate(int_has_0 = ifelse(int_ci_lb < 0 & int_ci_ub > 0, "contains 0", 
                            ifelse(int_ci_lb < 0 & int_ci_ub < 0, "below 0", 
                                   ifelse(int_ci_lb > 0 & int_ci_ub > 0, "above 0", "error"))), #change to 3 levels: above, below, contains
         year_has_0 = ifelse(year_ci_lb < 0 & year_ci_ub > 0, "contains 0", 
                             ifelse(year_ci_lb < 0 & year_ci_ub < 0, "below 0", 
                                    ifelse(year_ci_lb > 0 & year_ci_ub > 0, "above 0", "error"))),
         year_sq_has_0 = ifelse(year_sq_ci_lb < 0 & year_sq_ci_ub > 0, "contains 0", 
                                ifelse(year_sq_ci_lb < 0 & year_sq_ci_ub < 0, "below 0", 
                                       ifelse(year_sq_ci_lb > 0 & year_sq_ci_ub > 0, "above 0", "error"))))
AFT_CIs %>% 
  ggplot() +
  geom_pointrange(aes(x = id_arg, ymin = year_ci_lb, ymax = year_ci_ub, y = year_coef, color = year_has_0), alpha = 0.3) +
  facet_wrap(~ distribution)


AFT_CIs %>% 
  ggplot() +
  geom_pointrange(aes(x = id_arg, ymin = year_sq_ci_lb, ymax = year_sq_ci_ub, y = year_sq_coef, color = year_sq_has_0), alpha = 0.3) +
  facet_wrap(~ distribution)

AFT_CIs %>%   
  group_by(distribution, year_has_0) %>% 
  summarise(n = n()) %>% 
  mutate(prop_sig_year = n / sum(n)) %>% 
  filter(year_has_0 != "contains 0")



#logistic regresion
#this is not operational but this is how I approached finding the correct proportions at each time step, I can pull from the coefficient table to do this
cutpoint_log_scale <- log2(workbook$presets$MIC_breakpoint)

true_proportions_lr <- value_parameters %>% 
  mutate(
    total_R_prop = (1 - pnorm(cutpoint_log_scale, lower_means, lsd)) * lower_lambda + (1 - pnorm(cutpoint_log_scale, upper_means, usd)) * upper_lambda,
    component1_R_prop = (1- pnorm(cutpoint_log_scale, lower_means, lsd)) * lower_lambda,
    component2_R_prop = (1 - pnorm(cutpoint_log_scale, upper_means, usd)) * upper_lambda
  ) 





calculate_function <- function(i_year){
  #for a vector of years, do the above to generate a matrix of predicted values on logreg 
  
  workbook$lr_results$int_coef + workbook$lr_results$year_coef * i_year + workbook$lr_results$year_sq_coef * (i_year ^ 2)
}


convert_lr_proportion <- function(x){
  exp(x)/(1+exp(x))
}


tibble(as.data.frame(do.call(cbind,  map(c(0:(workbook$presets$nyears - 1)), calculate_function)))) %>% 
  map(., mean) %>% 
  map(., convert_lr_proportion) %>% 
  as.data.frame() %>% 
  transpose() %>% 
  tibble() %>% 
  rename(predicted_prop = V1) %>% 
  mutate(true_prop = true_proportions_lr$total_R_prop)



add_year_to_name <- function(x){
  #insert a vector of 0 to numyears-1
  paste("year_", x, sep = "")
}

year_vector <- c(0:(workbook$presets$nyears - 1))
year_names_for_plots<- map_chr(year_vector, add_year_to_name)

yearly_predicted_prop_lr <- tibble(as.data.frame(do.call(cbind,  map(c(0:(workbook$presets$nyears - 1)), calculate_function))))
names(yearly_predicted_prop_lr) <- year_names_for_plots

yearly_mean_predicted_prop_lr <- yearly_predicted_prop_lr %>% 
  pivot_longer(cols = 1:workbook$presets$nyears, names_to = "year", values_to = "proportion") %>% 
  group_by(year) %>% 
  summarise(mean = mean(proportion),
            median = median(proportion))

yearly_predicted_prop_lr %>% 
  pivot_longer(cols = 1:workbook$presets$nyears, names_to = "year", values_to = "proportion") %>% 
  ggplot() +
  geom_violin(aes(x = year, y = proportion, fill = year)) +
  geom_point(data = yearly_mean_predicted_prop_lr, aes(x = year, y = mean), color = "black") +
  geom_point(data = yearly_mean_predicted_prop_lr, aes(x = year, y = median), color = "white")



workbook$lr_results %>% 
  mutate(
    year_coef_lb = year_coef - 1.96 * year_se,
    year_coef_ub = year_coef + 1.96 * year_se,
    year_sq_coef_lb = year_sq_coef - 1.96 * year_sq_se,
    year_sq_coef_ub = year_sq_coef + 1.96 * year_sq_se
  ) %>% 
  mutate(
    year_ci_sig = ifelse(year_coef_lb > 0, "above zero",
                         ifelse(year_coef_ub < 0, "below zero",
                                "contains zero")
    )
  ) %>% 
  mutate(alpha_ci = ifelse(year_ci_sig == "contains zero", 0, 1)) %>% 
  ggplot() +
  geom_pointrange(aes(x = id_arg, ymin = year_coef_lb, ymax = year_coef_ub, y = year_coef, color = year_ci_sig, alpha = alpha_ci)) +
  scale_alpha(guide=FALSE)

workbook$lr_results %>% 
  mutate(
    year_coef_lb = year_coef - 1.96 * year_se,
    year_coef_ub = year_coef + 1.96 * year_se,
    year_sq_coef_lb = year_sq_coef - 1.96 * year_sq_se,
    year_sq_coef_ub = year_sq_coef + 1.96 * year_sq_se
  ) %>% 
  mutate(
    year_sq_ci_sig = ifelse(year_sq_coef_lb > 0, "above zero",
                            ifelse(year_sq_coef_ub < 0, "below zero",
                                   "contains zero")
    )
  ) %>% 
  mutate(alpha_sq_ci = ifelse(year_sq_ci_sig == "contains zero", 0, 1)) %>% 
  ggplot() +
  geom_pointrange(aes(x = id_arg, ymin = year_sq_coef_lb, ymax = year_sq_coef_ub, y = year_sq_coef, color = year_sq_ci_sig, alpha = alpha_sq_ci)) +
  scale_alpha(guide=FALSE)


workbook$lr_results %>% 
  mutate(
    year_coef_lb = year_coef - 1.96 * year_se,
    year_coef_ub = year_coef + 1.96 * year_se,
    year_sq_coef_lb = year_sq_coef - 1.96 * year_sq_se,
    year_sq_coef_ub = year_sq_coef + 1.96 * year_sq_se
  ) %>% 
  mutate(
    year_ci_sig = ifelse(year_coef_lb > 0, "above zero",
                         ifelse(year_coef_ub < 0, "below zero",
                                "contains zero")
    ),
    year_sq_ci_sig = ifelse(year_sq_coef_lb > 0, "above zero",
                            ifelse(year_sq_coef_ub < 0, "below zero",
                                   "contains zero"))
  ) %>% 
  group_by(year_ci_sig, year_sq_ci_sig) %>% 
  summarise(direction = n())

workbook$lr_results %>% 
  select(int_coef, year_coef, year_sq_coef) %>% 
  pivot_longer(cols = 1:3, names_to = "variable", values_to = "value") %>% 
  ggplot() +
  geom_boxplot(aes(x = variable, y = value))





#Differences and changes over time--------------------------------------------------------------------------------------
add_info_aft <- function(data, years_count){
  
  data %>% 
    mutate(last_value = intercept_coef + (year_coef * (years_count - 1)) + (year_sq_coef * ((years_count - 1)^2)) ) %>% 
    select( - c(ll_int, ll_full)) %>% 
    mutate(last_v_first = 
             ifelse(last_value > intercept_coef, "increase", 
                    ifelse(last_value < intercept_coef, "decrease", #warning("Last value is neither great than nor less than intercept")
                           "even")
             )
    ) %>% 
    mutate(int_ci_lb = intercept_coef - 1.96 * std_error_int, 
           int_ci_ub = intercept_coef + 1.96 * std_error_int,
           year_ci_lb = year_coef - 1.96 * std_error_year,
           year_ci_ub = year_coef + 1.96 * std_error_year,
           year_sq_ci_lb = year_sq_coef - 1.96 * std_error_year_sq,
           year_sq_ci_ub = year_sq_coef + 1.96 * std_error_year_sq) %>% 
    mutate(int_has_0 = ifelse(int_ci_lb < 0 & int_ci_ub > 0, "contains 0", 
                              ifelse(int_ci_lb < 0 & int_ci_ub < 0, "below 0", 
                                     ifelse(int_ci_lb > 0 & int_ci_ub > 0, "above 0", "error"))), #change to 3 levels: above, below, contains
           year_has_0 = ifelse(year_ci_lb < 0 & year_ci_ub > 0, "contains 0", 
                               ifelse(year_ci_lb < 0 & year_ci_ub < 0, "below 0", 
                                      ifelse(year_ci_lb > 0 & year_ci_ub > 0, "above 0", "error"))),
           year_sq_has_0 = ifelse(year_sq_ci_lb < 0 & year_sq_ci_ub > 0, "contains 0", 
                                  ifelse(year_sq_ci_lb < 0 & year_sq_ci_ub < 0, "below 0", 
                                         ifelse(year_sq_ci_lb > 0 & year_sq_ci_ub > 0, "above 0", "error")))) 
}

add_info_aft(workbook$aft_results, workbook$presets$nyears) %>% 
  group_by(year_has_0, year_sq_has_0, last_v_first) %>% 
  summarise(counts = n())

add_info_lr <- function(data_lr, year_count){
  data_lr %>% 
    mutate(
      year_coef_lb = year_coef - 1.96 * year_se,
      year_coef_ub = year_coef + 1.96 * year_se,
      year_sq_coef_lb = year_sq_coef - 1.96 * year_sq_se,
      year_sq_coef_ub = year_sq_coef + 1.96 * year_sq_se
    ) %>% 
    mutate(
      year_ci_sig = ifelse(year_coef_lb > 0, "above zero",
                           ifelse(year_coef_ub < 0, "below zero",
                                  "contains zero")
      ),
      year_sq_ci_sig = ifelse(year_sq_coef_lb > 0, "above zero",
                              ifelse(year_sq_coef_ub < 0, "below zero",
                                     "contains zero"))
    ) %>% 
    mutate(last_value = int_coef + (year_coef * (year_count - 1)) + (year_sq_coef * ((year_count - 1)^2))) %>% 
    mutate(last_v_first =  ifelse(last_value > int_coef, "increase", 
                                  ifelse(last_value < int_coef, "decrease", 
                                         "even")
    ))
}

add_info_lr(workbook$lr_results, workbook$presets$nyears) %>% 
  group_by(year_ci_sig, year_sq_ci_sig, last_v_first) %>% 
  summarise(direction = n())


add_info_aft(workbook$aft_results, workbook$presets$nyears) %>% 
  group_by(year_has_0, year_sq_has_0, last_v_first) %>% 
  summarise(counts = n(),
            mean_diff = mean(last_value - intercept_coef),
            median_diff = median(last_value - intercept_coef))


add_info_aft(workbook$aft_results, workbook$presets$nyears) %>% 
  ggplot() +
  geom_boxplot(aes(x = year_has_0, y = (last_value - intercept_coef), color = year_sq_has_0)) #+
#  facet_wrap(~last_v_first)



#boxplot year to year differences separated by significance of year and year_sq coefficients of aft
find_diff <- function(data, year_vector_with_no_0_year){
  data$year_coef + data$year_sq_coef * (year_vector_with_no_0_year ^ 2) - (data$year_sq_coef * ((year_vector_with_no_0_year - 1) ^ 2))
}

create_diff_names_seq <- function(df, vec){
  paste(paste(paste("y", vec, sep = ""), "toy", sep = ""),  vec - 1, sep  = "")
}


temp_tibble <- tibble(as.data.frame(do.call(cbind, map(1:(workbook$presets$nyears - 1), ~find_diff(workbook$aft_results, .x)))))


names(temp_tibble) <- map2_chr(temp_tibble, c(1:(workbook$presets$nyears - 1)), ~create_diff_names_seq(.x, .y))

add_info_aft(workbook$aft_results, workbook$presets$nyears) %>% 
  tibble(., temp_tibble) %>% 
  pivot_longer(c(map2_chr(temp_tibble, c(1:(workbook$presets$nyears - 1)), ~create_diff_names_seq(.x, .y))), names_to = "diff_type", values_to = "diff_value") %>% 
  ggplot() +
  geom_boxplot(aes(x = diff_type, y = diff_value, color = year_sq_has_0)) +
  facet_wrap(~year_has_0)


temp_tibble_2 <- tibble(as.data.frame(do.call(cbind, map(1:(workbook$presets$nyears - 1), ~find_diff(workbook$lr_results, .x)))))

names(temp_tibble_2) <- map2_chr(temp_tibble_2, c(1:(workbook$presets$nyears - 1)), ~create_diff_names_seq(.x, .y))

add_info_lr(workbook$lr_results, workbook$presets$nyears) %>% 
  tibble(., temp_tibble_2) %>% 
  pivot_longer(c(map2_chr(temp_tibble_2, c(1:(workbook$presets$nyears - 1)), ~create_diff_names_seq(.x, .y))), names_to = "diff_type", values_to = "diff_value") %>% 
  ggplot() +
  geom_boxplot(aes(x = diff_type, y = diff_value, color = year_sq_ci_sig)) +
  facet_wrap(~year_ci_sig)

add_info_aft(workbook$aft_results, workbook$presets$nyears) %>% 
  group_by(year_has_0, year_sq_has_0) %>% 
  summarise(n = n(),
            mean_year_coef = mean(year_coef),
            mean_year_sq_coef = mean(year_sq_coef))





calculate_year_predicted_aft <- function(data, year){
  data$intercept_coef + data$year_coef * year + data$year_sq_coef * (year ^ 2)
}

create_year_var_names_seq <- function(df, vec){
  paste("y", vec, sep = "")
}



temp_tibble_3 <- tibble(as.data.frame(do.call(cbind, map(c(0:(workbook$presets$nyears - 1)), ~calculate_year_predicted_aft(workbook$aft_results, .x)))))
names(temp_tibble_3) <- map2_chr(temp_tibble_3, c(0:(workbook$presets$nyears - 1)), ~create_year_var_names_seq(.x, .y))

add_info_aft(workbook$aft_results, workbook$presets$nyears) %>% 
  tibble(., temp_tibble_3) %>% 
  group_by(year_has_0, year_sq_has_0) %>% 
  summarise(n = n(),
            mean_year_coef = mean(year_coef),
            mean_year_sq_coef = mean(year_sq_coef))