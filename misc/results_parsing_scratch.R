file  <- gen_path_sim(location = location, format = format, array_name = array_name, date = date, i = i)

file <- "~/Desktop/june_2023/test_vars_06202023_2.Rdata"

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

batch_results %>% View

results <- batch_results$model_results[[1]]
parameters <- batch_results$settings

#results$single_model_output$likelihood
#
#results[[1]] #Error
#results #worked
#results$single_model_output$likelihood
#results$single_model_output$possible_data
#results$single_model_output$binom_model
#results$single_model_output$newmodel[[1]]
#results$single_model_output$newmodel[[2]]
#results$single_model_output$steps
#parameters$t_dist
#  #1: i
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



#π plotting-------------

#results$single_model_output$binom_model

#predict(results$single_model_output$binom_model, data.frame = t, type = "response")
#results$single_model_output$possible_data
#parameters$pi(t) %>% pull(2)

results$single_model_output$possible_data %>% filter(comp == c) %>%
  mutate(pi_hat = predict(results$single_model_output$binom_model, data.frame(t = t), type = "response")) %>%
  rowwise %>%
  mutate(pi_dgm = parameters$pi(t) %>% pull(2)) %>%
  ungroup %>%
  mutate(false_resid = pi_dgm - pi_hat,
         resid = (c == "2") * 1 - pi_hat ) %>%
  ggplot() +
  #  geom_point(aes(x = t, y = pi_hat, color = "Fitted Model")) +
    geom_function(fun = function(t){predict(results$single_model_output$binom_model, data.frame(t), type = "response")}, aes(color = "Fitted Model")) +
 #   geom_point(aes(x = t, y = pi_dgm, color = "Data Generating Mechanism")) +
  ggplot2::geom_function(fun = function(t){parameters$pi(t) %>% pull(2)}, aes(color = "Data Generating Mechanism")) +
    geom_smooth(aes(x = t, y = (c == "2") * 1))

#####ADD RUG TO PI PLOT

#results$single_model_output$newmodel



##Mu plotting---------------

#mu1 <- function(t){predict(results$single_model_output$newmodel[[1]], data.frame(t = t))}
#mu2 <- function(t){predict(results$single_model_output$newmodel[[2]], data.frame(t = t))}



mu.se <- function(t, c, z){predict(results$single_model_output$newmodel[[c]], data.frame(t = t)) + z * predict(results$single_model_output$newmodel[[c]], data.frame(t = t), se = TRUE)$se.fit}


results$single_model_output$possible_data %>% filter(comp == c) %>%
  mutate(mu_dgm = parameters$`E[X|T,C]`(t = t, c = c)) %>%
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
  ggplot() +
  #geom_point(aes(x = t, y = mu_dgm, color = c)) + #make as line: geom_function
  geom_function(fun = function(t){parameters$`E[X|T,C]`(t, c = 1)}, aes(color = "Component 1 Mu", linetype = "Data Generating Mechanism"), size = 0.9) +
geom_function(fun = function(t){parameters$`E[X|T,C]`(t, c = 2)}, aes(color = "Component 2 Mu", linetype = "Data Generating Mechanism"), size = 0.9) +
  geom_function(fun = function(t){predict(results$single_model_output$newmodel[[1]], data.frame(t = t))}, aes(color = "Component 1 Mu", linetype = "Fitted Model"), size = 0.9) +
  geom_function(fun = function(t){predict(results$single_model_output$newmodel[[2]], data.frame(t = t))}, aes(color = "Component 2 Mu", linetype = "Fitted Model"), size = 0.9) +
 # geom_point(aes(x = t, y = mu_hat, color = c), fill = "black", shape = 21) + #geom_function
  geom_point(aes(x = t, y = observed_value, color = c, fill = predicted_comp), alpha = 0.3, shape = 21) + #
 # geom_smooth(aes(x = t, y = observed_value), size = 0.3, alpha = 0.4, data = . %>% filter(c == "1")) +
 # geom_smooth(aes(x = t, y = observed_value), size = 0.3, alpha = 0.4, data = . %>% filter(c == "2")) +
  geom_hline(yintercept = parameters$low_con %>% unlist) +
  geom_hline(yintercept = parameters$high_con %>% unlist) +
  geom_function(fun = function(t){mu.se(t, c = 1, z = 1.96)}, aes(color = "Component 1 Mu", linetype = "Fitted Model SE"), size = 0.6, alpha = 0.6) +
  geom_function(fun = function(t){mu.se(t, c = 1, z = -1.96)}, aes(color = "Component 1 Mu", linetype = "Fitted Model SE"), size = 0.6, alpha = 0.6) +
  geom_function(fun = function(t){mu.se(t, c = 2, z = 1.96)}, aes(color = "Component 2 Mu", linetype = "Fitted Model SE"), size = 0.6, alpha = 0.6) +
  geom_function(fun = function(t){mu.se(t, c = 2, z = -1.96)}, aes(color = "Component 2 Mu", linetype = "Fitted Model SE"), size = 0.6, alpha = 0.6)





####Create notes on when dgm escapes SE bounds

##Plotting for data sets when EM for fit_model fails/errors out


###Oracle Stuff---------------

rhs <- stringr::str_split(parameters$formula2, pattern = '~', simplify = TRUE)[3,1]
##performance vs oracle model if true values kept?
oracle_bin_form <- paste("comp == '2'", rhs, sep = " ~ ") %>% as.formula

model_bin <- results$single_model_output$possible_data %>% gam::gam(oracle_bin_form, family = binomial(link = "logit"), data = .)


df_temp <- results$single_model_output$possible_data %>%
  mutate(
    `E[Y|t,c]` = case_when(
      c == "2" ~ 2 + 0.2*t, #3 + t + 2*t^2 - sqrt(t),
      c == "1" ~ -2 - 0.1*t,
      TRUE ~ NaN
    ),
    `sd[Y|t,c]` = case_when(
      c == "2" ~ 1, #3 + t + 2*t^2 - sqrt(t),
      c == "1" ~ 1,
      TRUE ~ NaN
    ),
    `P(Y|t,c)` =  if_else(
      left_bound == right_bound,
      dnorm(x = left_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`),
      pnorm(right_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`) -
        pnorm(left_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`)
    )
  ) %>%
  group_by(obs_id) %>%
  mutate(`P(C=c|t)` = case_when(
    c == "2" ~ predict(model_bin, newdata = tibble(t = t), type = "response"),
    c == "1" ~ 1 - predict(model_bin, newdata = tibble(t = t), type = "response")
  )) %>%
  mutate(`P(c,y|t)` = `P(C=c|t)` * `P(Y|t,c)`,
         `P(Y=y|t)` = sum(`P(c,y|t)`),
         `P(C=c|y,t)` = `P(c,y|t)` / `P(Y=y|t)`) %>%    ###COULD CHECK HOW WRONG THE WEIGHTS ARE
  ungroup()

formula = Surv(time = left_bound,
               time2 = right_bound,
               type = "interval2") ~ pspline(t, df = 0, calc = TRUE)

df_temp %>% filter(c == 1) -> df1
df_temp %>% filter(c == 2) -> df2
modelsplit_1 <- survival::survreg(
  parameters$formula,  ##Make this chunk into an argument of the function
  weights = `P(C=c|y,t)`,
  data = df1,
  dist = "gaussian",
  control = survreg.control(maxiter = maxiter_survreg, debug = verbose > 3))
modelsplit_2 <- survival::survreg(
  parameters$formula,  ##Make this chunk into an argument of the function
  weights = `P(C=c|y,t)`,
  data = df2,
  dist = "gaussian",
  control = survreg.control(maxiter = maxiter_survreg, debug = verbose > 3))



df_temp %>% filter(c == comp) %>%
mutate(
  oracle_predicted_comp = case_when(
    c == "2" & `P(C=c|y,t)` > 0.5 ~ "2",
    c == "2" & `P(C=c|y,t)` < 0.5 ~ "1",
    c == "1" & `P(C=c|y,t)` > 0.5 ~ "1",
    c == "1" & `P(C=c|y,t)` < 0.5 ~ "2",
    TRUE ~ "both"
  ) #rename observed data to underlying values
) %>%
  ggplot() +
  #geom_point(aes(x = t, y = mu_dgm, color = c)) + #make as line: geom_function
  geom_function(fun = function(t){parameters$`E[X|T,C]`(t, c = 1)}, aes(color = "Component 1 Mu DGM", linetype = "Data Generating Mechanism"), size = 0.9) +
  geom_function(fun = function(t){parameters$`E[X|T,C]`(t, c = 2)}, aes(color = "Component 2 Mu DGM", linetype = "Data Generating Mechanism"), size = 0.9) +
  geom_function(fun = function(t){predict(results$single_model_output$newmodel[[1]], data.frame(t = t))}, aes(color = "Component 1 Mu", linetype = "Fitted Model"), size = 0.9) +
  geom_function(fun = function(t){predict(results$single_model_output$newmodel[[2]], data.frame(t = t))}, aes(color = "Component 2 Mu", linetype = "Fitted Model"), size = 0.9) +
  geom_function(fun = function(t){predict(modelsplit_1, data.frame(t = t))}, aes(color = "Component 1 Mu", linetype = "Oracle Model"), size = 0.9) +
  geom_function(fun = function(t){predict(modelsplit_2, data.frame(t = t))}, aes(color = "Component 2 Mu", linetype = "Oracle Model"), size = 0.9) +
  # geom_point(aes(x = t, y = mu_hat, color = c), fill = "black", shape = 21) + #geom_function
  geom_point(aes(x = t, y = observed_value, color = c, fill = oracle_predicted_comp), alpha = 0.3, shape = 21) + #
  # geom_smooth(aes(x = t, y = observed_value), size = 0.3, alpha = 0.4, data = . %>% filter(c == "1")) +
  # geom_smooth(aes(x = t, y = observed_value), size = 0.3, alpha = 0.4, data = . %>% filter(c == "2")) +
  geom_hline(yintercept = parameters$low_con %>% unlist) +
  geom_hline(yintercept = parameters$high_con %>% unlist) +
  geom_function(fun = function(t){mu.se(t, c = 1, z = 1.96)}, aes(color = "Component 1 Mu", linetype = "Fitted Model SE"), size = 0.6, alpha = 0.6) +
  geom_function(fun = function(t){mu.se(t, c = 1, z = -1.96)}, aes(color = "Component 1 Mu", linetype = "Fitted Model SE"), size = 0.6, alpha = 0.6) +
  geom_function(fun = function(t){mu.se(t, c = 2, z = 1.96)}, aes(color = "Component 2 Mu", linetype = "Fitted Model SE"), size = 0.6, alpha = 0.6) +
  geom_function(fun = function(t){mu.se(t, c = 2, z = -1.96)}, aes(color = "Component 2 Mu", linetype = "Fitted Model SE"), size = 0.6, alpha = 0.6)



df_temp %>%
  mutate(
    oracle_pred = case_when(
      c == "1" ~ predict(modelsplit_1, data.frame(t = t)),
      c == "2" ~ predict(modelsplit_2, data.frame(t = t)),
      TRUE ~ NaN
    ),
    mu_hat = case_when(
      c == "1" ~ predict(results$single_model_output$newmodel[[1]], data.frame(t = t)),
      c == "2" ~ predict(results$single_model_output$newmodel[[2]], data.frame(t = t)),
      TRUE ~ NaN
    )
  ) %>%
  filter(c == comp) %>%
  mutate(oracle_resid =
           oracle_pred - mu_hat)




###Area Stuff-------------------










results$single_model_output$binom_model %>% summary
results$single_model_output$newmodel[[1]]$scale
results$single_model_output$newmodel[[2]]$scale


m <-
  functionBody(parameters$t_dist) %>% as.character() %>% stringr::str_match(., "min =\\s*(.*?)\\s*, max =")
t_min <- m[2,2] %>% as.numeric()

#m <- results[[7]][[2]][3] %>% as.character() %>% parse_number()
#results[[7]][[2]][3] %>% as.character() %>% gsub(".*min = .*", "", .)

  g <- functionBody(parameters$t_dist) %>% as.character() %>% stringr::str_split(., pattern = "max =", simplify = TRUE) %>% as.matrix() #%>% parse_number()
  t_max <- g[2,2] %>% parse_number()

t = seq(t_min, t_max, length.out = 100000)
width = (t_max - t_min)/100000


area_calc <-
  tibble(t) %>%
  reframe(.by = everything(),    #implement for other initial weighting options too ##########
          c = as.character(1:2)) %>%
  mutate(
    mu_hat = case_when(
      c == "1" ~ predict(results$single_model_output$newmodel[[1]], data.frame(t = t)),
      c == "2" ~ predict(results$single_model_output$newmodel[[2]], data.frame(t = t)),
      TRUE ~ NaN
    ),
    mu_dgm = parameters$`E[X|T,C]`(t = t, c = c),
    diff = mu_dgm - mu_hat,
    area = abs(diff) * width
  )
area_calc %>% group_by(c) %>%
  summarise(
    total_area = sum(area),
    avg_diff = mean(diff),
    med_diff = median(diff)
  )




area_calc_pi <-
  tibble(t) %>%
  mutate(pi_hat = predict(results$single_model_output$binom_model, data.frame(t = t), type = "response"),
         pi_dgm = parameters$pi(t) %>% pull("2"),
         diff = pi_dgm - pi_hat,
         area = abs(diff) * width)
area_calc_pi %>%
  summarise(
    total_area = sum(area),
    avg_diff = mean(diff),
    med_diff = median(diff)
  )

location <- "~/Desktop/june_2023/run_form2_loess_2"









#results$single_model_output$possible_data %>% filter(comp == c) %>%
#  mutate(mu_dgm = parameters$`E[X|T,C]`(t = t, c = c)) %>%
#  mutate(mu_hat = case_when(
#    c == "1" ~ predict(results$single_model_output$newmodel[[1]], data.frame(t = t)),
#    c == "2" ~ predict(results$single_model_output$newmodel[[2]], data.frame(t = t)),
#    TRUE ~ NaN
#  ),
#  mu_hat_se = case_when(
#    c == "1" ~ predict(results$single_model_output$newmodel[[1]], data.frame(t = t), se = TRUE)$se.fit,
#    c == "2" ~ predict(results$single_model_output$newmodel[[2]], data.frame(t = t), se = TRUE)$se.fit,
#    TRUE ~ NaN
#  )) %>% View
#


#predict(results$single_model_output$newmodel[[1]], data.frame(t = t), se = TRUE)$se.fit
#use geom_ribbon for ymin and ymax geom_function for mu_hat (for each half of model)


