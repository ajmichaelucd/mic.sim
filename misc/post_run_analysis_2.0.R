library(tidyr)
library(ggplot2)
library(patchwork)
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
number_of_batches = 10
#number per batch (e.g. 10)
number_per_batch = 10
#check by putting total number here
number_of_iterations = 100

#path to the directory full of the files
location <- "~/Desktop/july_2023/test_run_2"


#two formats i have used:
#name_date_number
#name_number_date
format <- "name_date_number"

#general name of simulation array
array_name <- "test_run_2"
date <- "07172023"

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
                            true_cens_2_rc = mean(true_cens_2_right),
                            cens_1_lc = mean(model_cens_1_left),
                            se_cens_1_lc = sd(model_cens_1_left),
                            true_cens_1_lc = mean(true_cens_1_left))

#batch_results<- loadRData("~/Desktop/june_2023/run_form2_loess_2/run_form2_loess_2_06132023_1.Rdata")
#results <- batch_results$model_results[[2]]
#results <- batch_results$model_results[[3]]

array_results %>% summarise(.by = scenario,
                            n = n())

array_results %>% View


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







iteration = 45
number_per_batch = 10

plot_iteration(iteration = iteration, number_per_batch = number_per_batch, analysis = NULL, location = location, format = format, array_name = array_name, date = date)


#function(iteration, number_per_batch){}
plot_iteration <- function(iteration, number_per_batch, analysis = NULL, location, format, array_name, date){
file  <- gen_path_sim(location = location, format = format, array_name = array_name, date = date, i = ceiling(iteration / number_per_batch))
batch_results <- loadRData(file)
iteration - number_per_batch * (ceiling(iteration / number_per_batch) - 1) -> i
batch_results$settings -> settings
batch_results$model_results[[i]] -> results

if(is.null(analysis)){
analysis <- case_when(
  length(results$single_model_output$single_model_output_fms) > 1 ~ "fms",
  length(results$single_model_output$single_model_output_fms) == 1 & length(results$single_model_output$single_model_output_fm) > 1 ~ "fm",
  TRUE ~ "nothing"
)
}

if(analysis == "fm"){
  single_model_output <- results$single_model_output$single_model_output_fm
} else if(analysis == "fms"){
  single_model_output <- results$single_model_output$single_model_output_fms
}else{
  single_model_output <- "nothing"
}
if(analysis %in% c("fm", "fms")){
pi_plot <- plot_pi(settings, single_model_output)


if(analysis == "fm"){
mu_plot <- plot_mu_fm(results, settings)
} else{
mu_plot <- plot_mu_fms(results, settings)
}

print(mu_plot/pi_plot)

} else{
  print(paste0("no results to show, no model convergence achieved for iteration ", iteration))
}

}
