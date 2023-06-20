file  <- gen_path_sim(location = location, format = format, array_name = array_name, date = date, i = i)

file <- "~/Desktop/june_2023/run_form2_loess_1_06132023_1.Rdata"

batch_results <- loadRData(file)




#if(i %in% reports){
#  a <- paste(rep("|", round(i / scaled)), collapse = "")
#  b <- paste(rep(".", 100 - round(i/scaled)), collapse = "")
#
#  paste0(paste0(a, b), "*") %>% print()
#}

#purrr::map(results, ~capture_error_measures_one_run(.x, intercepts, trends, sigma, pi_int, pi_trend, sigma_tolerance, pi_tolerance, intercepts_tolerance, trends_tolerance)) %>%
#  data.table::rbindlist()
##add

results %>% View

results[[1]] #Error
results[[7]] #worked
results[[7]][[1]]$likelihood
results[[7]][[1]]$possible_data
results[[7]][[1]]$binom_model
results[[7]][[1]]$newmodel[[1]]
results[[7]][[1]]$newmodel[[2]]
results[[7]][[1]]$steps
results[[7]][[2]]
  #1: i
  #2: n
  #3: t_dist
  #4: pi
  #5: `E[X|T,C`]
  #6: sd_vector
  #7: covariate_list
  #8: covariate_effect_vector
  #9: covariate_names
  #10: conc_limits_table
  #11: low_con
  #12: high_con
  #13: scale
  #14: formula
  #15: formula2
  #16: max_it
  #17: ncomp
  #18: tol_ll
  #19: maxiter_survreg
  #20: pi_link
  #21: verbose
  #22: allow_safety
  #23: cutoff
  #24: fms_only
  #25: initial_weighting
  #26: keep_true_value


results[[7]][[3]][1] #fms_only
results[[7]][[3]][2] #allow_safety
results[[7]][[3]][3] #fm_fail
results[[7]][[3]][4] #fms_fail





pi_plotting <- function(t){pi1(t)}

predict(binom_model, data.frame = t, type = "response")


results[[7]][[1]]$possible_data %>% filter(comp == c) %>%
  mutate(pi_hat = predict(results[[7]][[1]]$binom_model, data.frame(t = t), type = "response")) %>%
  rowwise %>%
  mutate(pi_dgm = results[[7]][[2]][[4]](t)[2]) %>%
  ungroup %>%
  mutate(false_resid = pi_dgm - pi_hat,
         resid = (c == "2") * 1 - pi_hat ) %>%
  ggplot() +
    geom_point(aes(x = t, y = pi_hat, color = "Fitted Model")) +
    geom_function(fun = function(t){predict(results[[7]][[1]]$binom_model, data.frame(t), type = "response")}, aes(color = "Fitted Model")) +
 #   geom_point(aes(x = t, y = pi_dgm, color = "Data Generating Mechanism")) +
  ggplot2::geom_function(fun = function(t){results[[7]][[2]][[4]] %>% pull("2")}, aes(color = "Data Generating Mechanism")) +
    geom_smooth(aes(x = t, y = (c == "2") * 1))

mu1 <- function(t){predict(results[[7]][[1]]$newmodel[[1]], data.frame(t = t))}
mu2 <- function(t){predict(results[[7]][[1]]$newmodel[[2]], data.frame(t = t))}


results[[7]][[1]]$possible_data %>% filter(comp == c) %>%
  mutate(mu_dgm = results[[7]][[2]][[5]](t = t, c = c)) %>%
  mutate(mu_hat = case_when(
    c == "1" ~ predict(results[[7]][[1]]$newmodel[[1]], data.frame(t = t)),
    c == "2" ~ predict(results[[7]][[1]]$newmodel[[2]], data.frame(t = t)),
    TRUE ~ NaN
  ),
  mu_hat_se = case_when(
    c == "1" ~ predict(results[[7]][[1]]$newmodel[[1]], data.frame(t = t), se = TRUE)$se.fit,
    c == "2" ~ predict(results[[7]][[1]]$newmodel[[2]], data.frame(t = t), se = TRUE)$se.fit,
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
  ggplot() +
  #geom_point(aes(x = t, y = mu_dgm, color = c)) + #make as line: geom_function
  geom_function(fun = function(t){results[[7]][[2]][[5]](t, c = 1)}, aes(color = "Component 1 Mu", linetype = "Data Generating Mechanism"), size = 0.9) +
geom_function(fun = function(t){results[[7]][[2]][[5]](t, c = 2)}, aes(color = "Component 2 Mu", linetype = "Data Generating Mechanism"), size = 0.9) +
  ggplot2::geom_function(fun = mu1, aes(color = "Component 1 Mu", linetype = "Fitted Model"), size = 0.9) +
  ggplot2::geom_function(fun = mu2, aes(color = "Component 2 Mu", linetype = "Fitted Model"), size = 0.9) +
 # geom_point(aes(x = t, y = mu_hat, color = c), fill = "black", shape = 21) + #geom_function
  geom_point(aes(x = t, y = observed_value, color = c, fill = predicted_comp), alpha = 0.3, shape = 21) + #
  geom_smooth(aes(x = t, y = observed_value), size = 0.3, alpha = 0.4, data = . %>% filter(c == "1")) +
  geom_smooth(aes(x = t, y = observed_value), size = 0.3, alpha = 0.4, data = . %>% filter(c == "2")) +
  geom_hline(yintercept = results[[7]][[2]][11] %>% unlist) +
  geom_hline(yintercept = results[[7]][[2]][12] %>% unlist)

#add confidence interval fot fitted model

results[[7]][[1]]$binom_model %>% summary
results[[7]][[1]]$newmodel[[1]]$scale
results[[7]][[1]]$newmodel[[2]]$scale


m <-
  results[[7]][[2]][3] %>% as.character() %>% str_match(., "min =\\s*(.*?)\\s*, max =")
t_min <- m[,2] %>% as.numeric()

#m <- results[[7]][[2]][3] %>% as.character() %>% parse_number()
#results[[7]][[2]][3] %>% as.character() %>% gsub(".*min = .*", "", .)
t_max <- results[[7]][[2]][3] %>% as.character() %>% gsub(".*max = ", "", .) %>% parse_number()

t = seq(t_min, t_max, length.out = 100000)
width = (t_max - t_min)/100000


area_calc <-
  tibble(t) %>%
  reframe(.by = everything(),    #implement for other initial weighting options too ##########
          c = as.character(1:2)) %>%
  mutate(
    mu_hat = case_when(
      c == "1" ~ predict(results[[7]][[1]]$newmodel[[1]], data.frame(t = t)),
      c == "2" ~ predict(results[[7]][[1]]$newmodel[[2]], data.frame(t = t)),
      TRUE ~ NaN
    ),
    mu_dgm = results[[7]][[2]][[5]](t = t, c = c),
    diff = mu_dgm - mu_hat,
    area = abs(diff) * width
  )
area_calc %>% group_by(c) %>%
  summarise(
    total_area = sum(area),
    avg_diff = mean(diff),
    med_diff = median(diff)
  )



results[[7]][[1]]$possible_data %>% filter(comp == c) %>%
  mutate(mu_dgm = results[[7]][[2]][[5]](t = t, c = c)) %>%
  mutate(mu_hat = case_when(
    c == "1" ~ predict(results[[7]][[1]]$newmodel[[1]], data.frame(t = t)),
    c == "2" ~ predict(results[[7]][[1]]$newmodel[[2]], data.frame(t = t)),
    TRUE ~ NaN
  ),
  mu_hat_se = case_when(
    c == "1" ~ predict(results[[7]][[1]]$newmodel[[1]], data.frame(t = t), se = TRUE)$se.fit,
    c == "2" ~ predict(results[[7]][[1]]$newmodel[[2]], data.frame(t = t), se = TRUE)$se.fit,
    TRUE ~ NaN
  )) %>% View



predict(results[[7]][[1]]$newmodel[[1]], data.frame(t = t), se = TRUE)$se.fit
#use geom_ribbon for ymin and ymax geom_function for mu_hat (for each half of model)


