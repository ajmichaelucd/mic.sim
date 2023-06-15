file  <- gen_path_sim(location = location, format = format, array_name = array_name, date = date, i = i)

file <- "~/Desktop/june_2023/run_form2_spline_1_06132023_1.Rdata"

results <- loadRData(file)




#if(i %in% reports){
#  a <- paste(rep("|", round(i / scaled)), collapse = "")
#  b <- paste(rep(".", 100 - round(i/scaled)), collapse = "")
#
#  paste0(paste0(a, b), "*") %>% print()
#}

purrr::map(results, ~capture_error_measures_one_run(.x, intercepts, trends, sigma, pi_int, pi_trend, sigma_tolerance, pi_tolerance, intercepts_tolerance, trends_tolerance)) %>%
  data.table::rbindlist()
##add

results %>% View

results[[1]] #Error
results[[3]] #worked
results[[3]][[1]]$likelihood
results[[3]][[1]]$possible_data
results[[3]][[1]]$binom_model
results[[3]][[1]]$newmodel[[1]]
results[[3]][[1]]$newmodel[[2]]
results[[3]][[1]]$steps
results[[3]][[2]]
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


results[[3]][[3]][1] #fms_only
results[[3]][[3]][2] #allow_safety
results[[3]][[3]][3] #fm_fail
results[[3]][[3]][4] #fms_fail





results[[8]][[1]]$possible_data %>% filter(comp == c) %>%
  mutate(pi_hat = predict(results[[8]][[1]]$binom_model, data.frame(t = t), type = "response")) %>%
  rowwise %>%
  mutate(pi_dgm = results[[8]][[2]][[4]](t)[2]) %>%
  ungroup %>%
  mutate(false_resid = pi_dgm - pi_hat,
         resid = (c == "2") * 1 - pi_hat ) %>%
  ggplot() +
    geom_point(aes(x = t, y = pi_hat)) +
    geom_point(aes(x = t, y = pi_dgm), color = "red") +
    geom_smooth(aes(x = t, y = (c == "2") * 1))


results[[8]][[1]]$possible_data %>% filter(comp == c) %>%
  mutate(mu_dgm = results[[8]][[2]][[5]](t = t, c = c)) %>%
  mutate(mu_hat = case_when(
    c == "1" ~ predict(results[[8]][[1]]$newmodel[[1]], data.frame(t = t)),
    c == "2" ~ predict(results[[8]][[1]]$newmodel[[2]], data.frame(t = t)),
    TRUE ~ NaN
  )) %>%
  mutate(resid = observed_value - mu_hat,
         false_resid = mu_dgm - mu_hat) %>%
  ggplot() +
  geom_point(aes(x = t, y = mu_dgm, color = c)) +
  geom_point(aes(x = t, y = mu_hat, color = c), fill = "black", shape = 21) +
  geom_point(aes(x = t, y = observed_value, color = c), alpha = 0.3) +
  geom_smooth(aes(x = t, y = observed_value), data = . %>% filter(c == "1")) +
  geom_smooth(aes(x = t, y = observed_value), data = . %>% filter(c == "2")) +
  geom_hline(yintercept = results[[8]][[2]][11] %>% unlist) +
  geom_hline(yintercept = results[[8]][[2]][12] %>% unlist)

results[[8]][[1]]$binom_model %>% summary
results[[8]][[1]]$newmodel[[1]]$scale
results[[8]][[1]]$newmodel[[2]]$scale


m <-
  results[[3]][[2]][3] %>% as.character() %>% str_match(., "min =\\s*(.*?)\\s*, max =")
t_min <- m[,2] %>% as.numeric()

#m <- results[[3]][[2]][3] %>% as.character() %>% parse_number()
#results[[3]][[2]][3] %>% as.character() %>% gsub(".*min = .*", "", .)
t_max <- results[[3]][[2]][3] %>% as.character() %>% gsub(".*max = ", "", .) %>% parse_number()

t = seq(t_min, t_max, length.out = 100000)
width = (t_max - t_min)/100000


area_calc <-
  tibble(t) %>%
  reframe(.by = everything(),    #implement for other intial weighting options too ##########
          c = as.character(1:2)) %>%
  mutate(
    mu_hat = case_when(
      c == "1" ~ predict(results[[8]][[1]]$newmodel[[1]], data.frame(t = t)),
      c == "2" ~ predict(results[[8]][[1]]$newmodel[[2]], data.frame(t = t)),
      TRUE ~ NaN
    ),
    mu_dgm = results[[8]][[2]][[5]](t = t, c = c),
    diff = mu_dgm - mu_hat,
    area = abs(diff) * width
  )
area_calc %>% group_by(c) %>%
  summarise(
    total_area = sum(area),
    avg_diff = mean(diff),
    med_diff = median(diff)
  )



