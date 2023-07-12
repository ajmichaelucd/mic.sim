library(tidyr)
library(ggplot2)
#new approach to analysis:

#load in 1 batch at a time (map over this)

#for each iteration in the 10, collect:
  #residuals for pi (both types)
  #residuals for mu (by component) (both types)
  #oracle vs fitted model mu residuals-like thing
  #pct of obs classified correctly
  #area btwn mu_fitted and dgm
  #area btwn pi_fitted and dgm

###Parameters to estimate and other simulation info----------------------
#number of batches (e.g. 100)
number_of_batches = 100
#number per batch (e.g. 10)
number_per_batch = 10
#check by putting total number here
number_of_iterations = 1000

#path to the directory full of the files
location <- "~/Desktop/june_2023/run_form2_spline_2"


#two formats i have used:
#name_date_number
#name_number_date
format <- "name_date_number"

#general name of simulation array
array_name <- "run_form2_spline_2"
date <- "06132023"

incomplete <- check_array_complete(number_of_batches = number_of_batches, format = format, location = location, array_name = array_name, date = date)

print(incomplete)

##if we have an incomplete array, let's do reruns of those batches locally
if(nrow(tibble(incomplete)) > 1 |  (nrow(tibble(incomplete))  == 1 && incomplete != "All Clear")){
  ###start by getting the parameters to do reruns if needed
    ##select one of the batches that did run to grab the parameters
i = tibble(complete = 1:number_of_batches) %>% filter(!complete %in% (incomplete %>% pull)) %>% pull %>% sample(., size = 1, replace = FALSE)
      #load in that completed batch
file  <- gen_path_sim(location = location, format = format, array_name = array_name, date = date, i = i)
batch_results <- loadRData(file)

      #save the settings from the batch to use for reruns of other batches
rerun_parameters <- batch_results$settings

##grabbed the parameters, now to rerun these locally:
 ######NEEDS ADDITION HERE
##function to map over all batches
    ##function to map over all iterations
        ##function to fit locally using parameters provided from above passed through the other two functions (except for $iteration_set)

rerun_incomplete_sets(
  location = location,
  incomplete = incomplete,
  number_per_batch = number_per_batch,
  array_name = array_name,
  date = date,
  rerun_parameters = rerun_parameters
)

setwd(
 "~/Desktop/Dissertation Project/Chapter 1/mic.sim"
)

}


incomplete <- check_array_complete(number_of_batches = number_of_batches, format = format, location = location, array_name = array_name, date = date)

print(incomplete)


array_results <-
  purrr::map(
    1:number_of_batches,
    ~ capture_error_measures_one_batch(
      location = location,
      format = format,
      array_name = array_name,
      date = date,
      i = .x,
      number_of_batches = number_of_batches
    )
  ) %>%
  rbindlist() %>% tibble()


array_results %>% group_by(cross) %>% summarise(n = n())

array_results %>% summarise(.by = scenario, n = n())

array_results %>% summarise(.by = scenario,
                            cens_2_rc = mean(model_cens_2_right),
                            se_cens_2_rc = sd(model_cens_2_right),
                            true_cens_2_rc = mean(true_cens_2_right))

#batch_results<- loadRData("~/Desktop/june_2023/run_form2_loess_2/run_form2_loess_2_06132023_1.Rdata")
#results <- batch_results$model_results[[2]]
#results <- batch_results$model_results[[3]]






save(array_results, file = "~/Desktop/june_2023/analysis/run_form2_spline_2_06132023_results.Rdata")












#save(array_results, file = "~/Desktop/june_2023/analysis/run_form2_loess_form1_main_2_06132023_results.Rdata")

run_form2_spline_1_results <- loadRData("~/Desktop/june_2023/analysis/run_form2_spline_1_06132023_results.Rdata")
run_form2_loess_1_results <- loadRData("~/Desktop/june_2023/analysis/run_form2_loess_1_06132023_results.Rdata")
run_form2_spline_2_results<- loadRData("~/Desktop/june_2023/analysis/run_form2_spline_2_06132023_results.Rdata")

#run_form2_loess_form1_main_2_results <- loadRData("~/Desktop/june_2023/analysis/run_form2_loess_form1_main_2_06132023_results.Rdata")

df <- rbind(
run_form2_spline_1_results %>% mutate(form = "spline_1"),
run_form2_loess_1_results %>% mutate(form = "loess_1"), #%>% ggplot() + geom_histogram(aes(x = steps)) + xlim(0, 100)
run_form2_spline_2_results %>% mutate(form = "spline_2") #%>% ggplot() + geom_histogram(aes(x = steps)) + xlim(0, 100)
)


df %>% summarise(.by = c(form, scenario),
                steps = mean(steps),
                 mu_resid_sq_1 = mean(mu_resid_sq_1),
                 mu_resid_sq_2 = mean(mu_resid_sq_2),
                 mu_resid_sq_both = mean(mu_resid_sq_both),
                 mu_false_resid_sq_1 = mean(mu_false_resid_sq_1),
                 mu_false_resid_sq_2 = mean(mu_false_resid_sq_2),
                 mu_false_resid_sq_both = mean(mu_false_resid_sq_both),
                 mu_bias_1 = mean(mu_bias_1),
                 mu_bias_2 = mean(mu_bias_2),
                 mu_bias_both = mean(mu_bias_both),
                 pi_resid_abs = mean(pi_resid_abs),
                 pi_resid_sq = mean(pi_resid_sq),
                 pi_false_resid_sq = mean(pi_false_resid_sq),
                 pi_bias = mean(pi_bias),
                 c1_scale = mean(c1_scale),
                 c2_scale = mean(c2_scale),
                 model_cens_1_interval = mean(model_cens_1_interval),
                 model_cens_1_left = mean(model_cens_1_left),
                 model_cens_1_right = mean(model_cens_1_right),
                 model_cens_2_interval = mean(model_cens_2_interval),
                 model_cens_2_left = mean(model_cens_2_left),
                 model_cens_2_right = mean(model_cens_2_right),
                 true_cens_1_interval = mean(true_cens_1_interval),
                 true_cens_1_left = mean(true_cens_1_left),
                 true_cens_1_right = mean(true_cens_1_right),
                 true_cens_2_interval = mean(true_cens_2_interval),
                 true_cens_2_left = mean(true_cens_2_left),
                 true_cens_2_right = mean(true_cens_2_right),
                 total_area_c1 = mean(total_area_c1),
                 total_area_c2 = mean(total_area_c2),
                 avg_diff_c1 = mean(avg_diff_c1),
                 avg_diff_c2 = mean(avg_diff_c2),
                 med_diff_c1 = mean(med_diff_c1),
                 med_diff_c2 = mean(med_diff_c2),
                 total_area_pi = mean(total_area_pi),
                 avg_diff_pi = mean(avg_diff_pi),
                 med_diff_pi = mean(med_diff_pi)
                 ) %>% View


df %>%
  mutate(grouping = paste0(form, "_", scenario)) %>%
  filter(grouping %in% c("loess_1_fm", "spline_1_fm")) %>%
  ggplot() +
  geom_histogram(aes(x = c2_scale, fill = grouping), bins = 60) +
  facet_grid(grouping ~ 1) + xlim(0, 15) +
  geom_vline(xintercept = 1) +
  geom_vline(xintercept = 1.5, alpha = 0.4)



df %>%
  mutate(grouping = paste0(form, "_", scenario)) %>%
  ggplot() +
  geom_histogram(aes(x = mu_resid_sq_2, fill = grouping,
                     y=after_stat(c(
                       count[group==1]/sum(count[group==1]),
                       count[group==2]/sum(count[group==2]),
                       count[group==3]/sum(count[group==3]),
                       count[group==4]/sum(count[group==4]),
                       count[group==5]/sum(count[group==5])
                     ))
                     )) +
  facet_grid(grouping ~ 1) + xlim(-1, 10)

df %>%
  mutate(grouping = paste0(form, "_", scenario)) %>%
  ggplot() +
  geom_histogram(aes(x = pi_resid_sq, fill = grouping,
                     y=after_stat(c(
                       count[group==1]/sum(count[group==1]),
                       count[group==2]/sum(count[group==2]),
                       count[group==3]/sum(count[group==3]),
                       count[group==4]/sum(count[group==4]),
                       count[group==5]/sum(count[group==5])
                     ))
  )) +
  facet_grid(grouping ~ 1)


df %>% filter(scenario == "fm") %>%
  ggplot() +
  geom_histogram(aes(x = model_cens_2_right, fill = form)) +
  facet_grid(form ~ 1)

df %>% mutate(would_not_pass =
                case_when(model_cens_2_right > 0.85 &
                            scenario == "fm" ~ TRUE,
                          TRUE ~ FALSE)) %>% summarise(.by = c(form, scenario, would_not_pass),
                                                       n = n())








iteration = 8
number_per_batch = 10

#function(iteration, number_per_batch){}

file  <- gen_path_sim(location = location, format = format, array_name = array_name, date = date, i = ceiling(iteration / number_per_batch))
batch_results <- loadRData(file)
iteration - number_per_batch * (ceiling(iteration / number_per_batch) - 1) -> i
batch_results$settings -> settings
batch_results$model_results[[i]] -> results


plot_pi <- function(results, settings){
results$single_model_output$possible_data %>% filter(comp == c) %>%
  mutate(pi_hat = predict(results$single_model_output$binom_model, data.frame(t = t), type = "response")) %>%
  rowwise %>%
  mutate(pi_dgm = settings$pi(t) %>% pull(2)) %>%
  ungroup %>%
  mutate(false_resid = pi_dgm - pi_hat,
         resid = (c == "2") * 1 - pi_hat ) %>%
  ggplot() +
  #  geom_point(aes(x = t, y = pi_hat, color = "Fitted Model")) +
  geom_function(fun = function(t){predict(results$single_model_output$binom_model, data.frame(t), type = "response")}, aes(color = "Fitted Model")) +
  #   geom_point(aes(x = t, y = pi_dgm, color = "Data Generating Mechanism")) +
  ggplot2::geom_function(fun = function(t){settings$pi(t) %>% pull(2)}, aes(color = "Data Generating Mechanism")) +
  geom_smooth(aes(x = t, y = (c == "2") * 1), formula = y ~ x, method = "loess")
}

plot_pi(results, settings)



mu.se <- function(t, c, z){predict(results$single_model_output$newmodel[[c]], data.frame(t = t)) + z * predict(results$single_model_output$newmodel[[c]], data.frame(t = t), se = TRUE)$se.fit}
mu.se.fms <- function(t, z){predict(results$single_model_output$newmodel, data.frame(t = t)) + z * predict(results$single_model_output$newmodel, data.frame(t = t), se = TRUE)$se.fit}



plot_mu_fm <- function(results, settings){
results$single_model_output$possible_data %>%
  mutate(mu_dgm = settings$`E[X|T,C]`(t = t, c = c)) %>%
  mutate(mu_hat = case_when(
    c == "1" ~ predict(results$single_model_output$newmodel[[1]], data.frame(t = t)),
    c == "2" ~ predict(results$single_model_output$newmodel[[2]], data.frame(t = t)),
    TRUE ~ NaN
  ),
  mu_hat_se = case_when(
    c == "1" ~ predict(results$single_model_output$newmodel[[1]], data.frame(t = t), se = TRUE)$se.fit,
    c == "2" ~ predict(results$single_model_output$newmodel[[2]], data.frame(t = t), se = TRUE)$se.fit,
    TRUE ~ NaN
  )) %>%
  mutate(resid = observed_value - mu_hat,
         false_resid = mu_dgm - mu_hat) %>%
  mutate(
    predicted_comp = case_when(
      c == "2" & `P(C=c|y,t)` > 0.5 ~ "2",
      c == "2" & `P(C=c|y,t)` < 0.5 ~ "1",
      c == "1" & `P(C=c|y,t)` > 0.5 ~ "1",
      c == "1" & `P(C=c|y,t)` < 0.5 ~ "2",
      TRUE ~ "both"
    ) #rename observed data to underlying values
  ) %>%
    filter(c == "2") %>%
  ggplot() +
  #geom_point(aes(x = t, y = mu_dgm, color = c)) + #make as line: geom_function
  geom_function(fun = function(t){settings$`E[X|T,C]`(t, c = 1)}, aes(color = "Component 1 Mu", linetype = "Data Generating Mechanism"), size = 0.9) +
  geom_function(fun = function(t){settings$`E[X|T,C]`(t, c = 2)}, aes(color = "Component 2 Mu", linetype = "Data Generating Mechanism"), size = 0.9) +
  geom_function(fun = function(t){predict(results$single_model_output$newmodel[[1]], data.frame(t = t))}, aes(color = "Component 1 Mu", linetype = "Fitted Model"), size = 0.9) +
  geom_function(fun = function(t){predict(results$single_model_output$newmodel[[2]], data.frame(t = t))}, aes(color = "Component 2 Mu", linetype = "Fitted Model"), size = 0.9) +
  # geom_point(aes(x = t, y = mu_hat, color = c), fill = "black", shape = 21) + #geom_function
  geom_smooth(aes(x = t, y = observed_value), size = 0.3, alpha = 0.8, data = . %>% filter(comp == "1"), se = FALSE, span = 0.5, color = "darkgreen", formula = y ~ x, method = "loess") +  ###fix later
  geom_smooth(aes(x = t, y = observed_value), size = 0.3, alpha = 0.8, data = . %>% filter(comp == "2"), se = FALSE, span = 0.5, color = "darkgreen", formula = y ~ x, method = "loess") + ###fix later
  geom_hline(yintercept = settings$low_con %>% unlist) +
  geom_hline(yintercept = settings$high_con %>% unlist) +
  geom_function(fun = function(t){mu.se(t, c = 1, z = 1.96)}, aes(color = "Component 1 Mu", linetype = "Fitted Model SE"), size = 0.6, alpha = 0.6) +
  geom_function(fun = function(t){mu.se(t, c = 1, z = -1.96)}, aes(color = "Component 1 Mu", linetype = "Fitted Model SE"), size = 0.6, alpha = 0.6) +
  geom_function(fun = function(t){mu.se(t, c = 2, z = 1.96)}, aes(color = "Component 2 Mu", linetype = "Fitted Model SE"), size = 0.6, alpha = 0.6) +
  geom_function(fun = function(t){mu.se(t, c = 2, z = -1.96)}, aes(color = "Component 2 Mu", linetype = "Fitted Model SE"), size = 0.6, alpha = 0.6) +
  ggnewscale::new_scale_color() +
  geom_point(aes(x = t, y = observed_value, color = `P(C=c|y,t)`, shape = comp), alpha = 0.3) +
  ggplot2::scale_colour_gradientn(colours = c("purple", "orange"))
}



plot_mu_fm(results, settings)

plot_mu_fms <- function(results, settings){ ##change later once i update fms to save length 2 list
  results$single_model_output$possible_data %>% #filter(c == "2") %>%
    mutate(mu_dgm = settings$`E[X|T,C]`(t = t, c = c)) %>%
    mutate(mu_hat = case_when(
      c == "1" ~ predict(results$single_model_output$newmodel, data.frame(t = t)),
      c == "2" ~ NaN,
      TRUE ~ NaN
    ),
    mu_hat_se = case_when(
      c == "1" ~ predict(results$single_model_output$newmodel, data.frame(t = t), se = TRUE)$se.fit,
      c == "2" ~ NaN,
      TRUE ~ NaN
    )) %>%
    mutate(resid = observed_value - mu_hat,
           false_resid = mu_dgm - mu_hat) %>%
    mutate(
      predicted_comp = case_when(
        c == "2" & `P(C=c|y,t)` > 0.5 ~ "2",
        c == "2" & `P(C=c|y,t)` < 0.5 ~ "1",
        c == "1" & `P(C=c|y,t)` > 0.5 ~ "1",
        c == "1" & `P(C=c|y,t)` < 0.5 ~ "2",
        TRUE ~ "both"
      ) #rename observed data to underlying values
    ) %>% ###add a variable for misclassified observations maybe?
    filter(c == "2") %>%
    ggplot() +
    #geom_point(aes(x = t, y = mu_dgm, color = c)) + #make as line: geom_function
    geom_function(fun = function(t){settings$`E[X|T,C]`(t, c = 1)}, aes(color = "Component 1 Mu", linetype = "Data Generating Mechanism"), size = 0.9) +
    geom_function(fun = function(t){settings$`E[X|T,C]`(t, c = 2)}, aes(color = "Component 2 Mu", linetype = "Data Generating Mechanism"), size = 0.9) +
    geom_function(fun = function(t){predict(results$single_model_output$newmodel, data.frame(t = t))}, aes(color = "Component 1 Mu", linetype = "Fitted Model"), size = 0.9) +
    #geom_function(fun = function(t){predict(results$single_model_output$newmodel[[2]], data.frame(t = t))}, aes(color = "Component 2 Mu", linetype = "Fitted Model"), size = 0.9) +
    # geom_point(aes(x = t, y = mu_hat, color = c), fill = "black", shape = 21) + #geom_function
    geom_smooth(aes(x = t, y = observed_value), size = 0.3, alpha = 0.8, data = . %>% filter(comp == "1"), se = FALSE, span = 0.5, color = "darkgreen", formula = y ~ x, method = "loess") +  ###fix later
    geom_smooth(aes(x = t, y = observed_value), size = 0.3, alpha = 0.8, data = . %>% filter(comp == "2"), se = FALSE, span = 0.5, color = "darkgreen", formula = y ~ x, method = "loess") + ###fix later
    geom_hline(yintercept = settings$low_con %>% unlist) +
    geom_hline(yintercept = settings$high_con %>% unlist) +
    geom_function(fun = function(t){mu.se.fms(t, z = 1.96)}, aes(color = "Component 1 Mu", linetype = "Fitted Model SE"), size = 0.6, alpha = 0.6) +
    geom_function(fun = function(t){mu.se.fms(t, z = -1.96)}, aes(color = "Component 1 Mu", linetype = "Fitted Model SE"), size = 0.6, alpha = 0.6) +
    #geom_function(fun = function(t){mu.se(t, c = 2, z = 1.96)}, aes(color = "Component 2 Mu", linetype = "Fitted Model SE"), size = 0.6, alpha = 0.6) +
    #geom_function(fun = function(t){mu.se(t, c = 2, z = -1.96)}, aes(color = "Component 2 Mu", linetype = "Fitted Model SE"), size = 0.6, alpha = 0.6) +
    ggnewscale::new_scale_color() +
    geom_point(aes(x = t, y = observed_value, color = `P(C=c|y,t)`, shape = comp), alpha = 0.3) +
    ggplot2::scale_colour_gradientn(colours = c("purple", "orange"))
}
