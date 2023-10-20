#RUN WHEN TULATH-PM STOPS AT ITERATION 16 BECAUSE LL DECREASED
#OR TILMIC-PM AT ITERATION 6
    #TRIED TO SWITCH TILMIC TO NS INSTEAD OF PSPLINE AND THE ISSUE STILL HAPPENED, JUST AT A DIFFERENT ITERATION (18)

##WHAT DIRECTION (IN TERMS OF LL) ARE MOST OF THE OBS GOING IN ITERATION 16 RELATIVE TO WHAT THEY WERE IN 15?
left_join(
possible_data %>% summarise(.by = obs_id, likelihood_i = sum(`P(c,y|t)`)) %>% mutate(log_likelihood_i = log(likelihood_i)),
possible_data_old %>% summarise(.by = obs_id, likelihood_i_old = sum(`P(c,y|t)`)) %>% mutate(log_likelihood_i_old = log(likelihood_i_old)),
by = "obs_id") %>% mutate(increase = log_likelihood_i - log_likelihood_i_old,
                          sign = case_when(
                            increase > 0 ~ "+",
                            increase < 0 ~ "-",
                            TRUE ~ "="
                          )) %>% #View
summarise(.by = sign, n = n(),
          sum = sum(increase),
          ll = sum(log_likelihood_i))

##LETS PLOT THOSE CHANGES, BLUE IS GOOD (LL INCREASED 15 -> 16), RED IS BAD
left_join(
  possible_data %>% summarise(.by = obs_id, likelihood_i = sum(`P(c,y|t)`)) %>% mutate(log_likelihood_i = log(likelihood_i)),
  possible_data_old %>% summarise(.by = obs_id, likelihood_i_old = sum(`P(c,y|t)`)) %>% mutate(log_likelihood_i_old = log(likelihood_i_old)),
  by = "obs_id") %>% mutate(increase = log_likelihood_i - log_likelihood_i_old,
                            sign = case_when(
                              increase > 0 ~ "+",
                              increase < 0 ~ "-",
                              TRUE ~ "="
                            )) %>%
  ggplot() + geom_histogram(
    aes(x = increase, fill = sign), bins = 20, boundary = 0, color = "black"
  )

##CHECK IF WEIGHTS SHOW A PATTERN
left_join(
  possible_data %>% summarise(.by = obs_id, likelihood_i = sum(`P(c,y|t)`)) %>% mutate(log_likelihood_i = log(likelihood_i)),
  possible_data_old %>% summarise(.by = obs_id, likelihood_i_old = sum(`P(c,y|t)`)) %>% mutate(log_likelihood_i_old = log(likelihood_i_old)),
  by = "obs_id") %>% mutate(increase = log_likelihood_i - log_likelihood_i_old,
                            sign = case_when(
                              increase > 0 ~ "+",
                              increase < 0 ~ "-",
                              TRUE ~ "="
                            )) %>%
  left_join(., possible_data %>% filter(c == 2) %>% select(obs_id, cens, `P(C=c|y,t)`) , by = "obs_id") %>%
  left_join(., possible_data_old %>% filter(c == 2) %>% select(obs_id, `P(C=c|y,t)`) %>% rename(`P(C=c|y,t)old` = `P(C=c|y,t)`), by = "obs_id") %>%
  ggplot() +
  #geom_histogram(
  #  aes(x = `P(C=c|y,t)`, fill = sign), bins = 20, boundary = 0, color = "black"
  #)
  geom_point(aes(x = `P(C=c|y,t)`, y = increase, color = sign, shape = cens), alpha = 0.2)

#MORE OF THE BLUES, BUT THE REDS ARE ON AVERAGE MUCH LARGER ABSOLUTE VALUE for Tulath
left_join(
  possible_data %>% summarise(.by = obs_id, likelihood_i = sum(`P(c,y|t)`)) %>% mutate(log_likelihood_i = log(likelihood_i)),
  possible_data_old %>% summarise(.by = obs_id, likelihood_i_old = sum(`P(c,y|t)`)) %>% mutate(log_likelihood_i_old = log(likelihood_i_old)),
  by = "obs_id") %>% mutate(increase = log_likelihood_i - log_likelihood_i_old,
                            sign = case_when(
                              increase > 0 ~ "+",
                              increase < 0 ~ "-",
                              TRUE ~ "="
                            )) %>%
  ggplot() + geom_histogram(
    aes(x = abs(increase), fill = sign), bins = 20, boundary = 0, color = "black"
)

#Most of the decreases are also from observations that have more negative log likelihoods overall for TULATH, opposite for TILMIC
left_join(
  possible_data %>% summarise(.by = obs_id, likelihood_i = sum(`P(c,y|t)`)) %>% mutate(log_likelihood_i = log(likelihood_i)),
  possible_data_old %>% summarise(.by = obs_id, likelihood_i_old = sum(`P(c,y|t)`)) %>% mutate(log_likelihood_i_old = log(likelihood_i_old)),
  by = "obs_id") %>% mutate(increase = log_likelihood_i - log_likelihood_i_old,
                            sign = case_when(
                              increase > 0 ~ "+",
                              increase < 0 ~ "-",
                              TRUE ~ "="
                            )) %>%
  ggplot() + geom_histogram(
    aes(x = log_likelihood_i, fill = sign), bins = 20, color = "black"
  )







#let's add the directional change in log-likelihood to possible data
left_join(
  possible_data %>% summarise(.by = obs_id, likelihood_i = sum(`P(c,y|t)`)) %>% mutate(log_likelihood_i = log(likelihood_i)),
  possible_data_old %>% summarise(.by = obs_id, likelihood_i_old = sum(`P(c,y|t)`)) %>% mutate(log_likelihood_i_old = log(likelihood_i_old)),
  by = "obs_id") %>% mutate(increase = log_likelihood_i - log_likelihood_i_old,
                            sign = case_when(
                              increase > 0 ~ "+",
                              increase < 0 ~ "-",
                              TRUE ~ "="
                            )) %>% #filter(sign == "-") %>%
  View

left_join(
  possible_data %>% summarise(.by = obs_id, likelihood_i = sum(`P(c,y|t)`)) %>% mutate(log_likelihood_i = log(likelihood_i)),
  possible_data_old %>% summarise(.by = obs_id, likelihood_i_old = sum(`P(c,y|t)`)) %>% mutate(log_likelihood_i_old = log(likelihood_i_old)),
  by = "obs_id") %>% mutate(increase = log_likelihood_i - log_likelihood_i_old,
                            sign = case_when(
                              increase > 0 ~ "+",
                              increase < 0 ~ "-",
                              TRUE ~ "="
                            )) %>% filter(sign == "-") %>% pull(obs_id) -> decs


possible_data %>% mutate(decrease = case_when(
  obs_id %in% decs ~ "decreased",
  TRUE ~ "increased")
) %>% View

#is there a pattern in which values of y have the decreases? check mics
possible_data %>% mutate(decrease = case_when(
  obs_id %in% decs ~ "decreased",
  TRUE ~ "increased")
) %>% filter(c== 2) %>% ggplot() +
  geom_bar(aes(x = mic_column, fill = decrease), stat = "count", color = "black")

#is there a pattern in which values of y have the decreases? check t
possible_data %>% mutate(decrease = case_when(
  obs_id %in% decs ~ "decreased",
  TRUE ~ "increased")
) %>% filter(c== 2) %>% ggplot() +
  geom_histogram(aes(x = t, fill = decrease), color = "black", binwidth = 1)



#for tilmic: decreases are weighted towards component 1
possible_data %>% mutate(decrease = case_when(
     obs_id %in% decs ~ "decreased",
     TRUE ~ "increased")
   ) %>% summarise(.by = c(decrease, c), mean_weight = mean(`P(C=c|y,t)`), sd_weight = sd(`P(C=c|y,t)`), median_weight = median(`P(C=c|y,t)`))

possible_data_old %>% mutate(decrease = case_when(
  obs_id %in% decs ~ "decreased",
  TRUE ~ "increased")
) %>% summarise(.by = c(decrease, c), mean_weight = mean(`P(C=c|y,t)`), sd_weight = sd(`P(C=c|y,t)`), median_weight = median(`P(C=c|y,t)`))


#let's plot and see which observations are getting worse
possible_data %>% mutate(decrease = case_when(
  obs_id %in% decs ~ "decreased",
  TRUE ~ "increased")) %>%
    mutate(upper = case_when(
      right_bound == Inf ~ left_bound + 2.5,
      TRUE ~ right_bound
    ),
    lower = case_when(
      left_bound == -Inf ~ right_bound - 2.5,
      TRUE ~ left_bound)) %>%
    ggplot() +
    geom_segment(aes(
      x = t, xend = t, y = lower, yend = upper, color = decrease
    ))


##So either the likelihood or weights for component 2 is worse for the high weight observations
rbind(possible_data %>% filter(obs_id %in% decs) %>% select(obs_id, left_bound, right_bound, `P(c,y|t)`, c, `P(C=c|y,t)`, `E[Y|t,c]`) %>% mutate(iter = 16),
possible_data_old %>% filter(obs_id %in% decs) %>% select(obs_id, left_bound, right_bound, `P(c,y|t)`, c, `P(C=c|y,t)`, `E[Y|t,c]`) %>% mutate(iter = 15)) %>%
  select(obs_id, iter, c, left_bound, right_bound, everything()) %>% arrange(obs_id) %>% #filter(c == 2) %>%
  pivot_wider(id_cols = c(obs_id, c, left_bound, right_bound), names_from = iter, values_from = `P(c,y|t)`:`E[Y|t,c]`) %>%
  mutate(
    `delta_P(c,y|t)` = `P(c,y|t)_16` - `P(c,y|t)_15`,
    `delta_P(C=c|y,t)` = `P(C=c|y,t)_16` - `P(C=c|y,t)_15`,
    `delta_E[Y|t,c]` = `E[Y|t,c]_16` - `E[Y|t,c]_15`
  ) %>% select(obs_id:right_bound, `delta_P(c,y|t)`:`delta_E[Y|t,c]`) %>% View


plot_fm_step(pi_model_old, mu_models_old, 2, possible_data_old, FALSE, i-1)
plot_fm_step(pi_model_new, mu_models_new, 2, possible_data, FALSE, i)



##tried to stratify here, didn't work because the plots don't wiggle
mm = possible_data %>% filter(`P(C=c|y,t)` > 0) %>%
  survival::survreg(
    formula = Surv(time = left_bound,
                   time2 = right_bound,
                   #type = "interval2") ~ ns(t, df = 10)+survival::strata(c),
                   type = "interval2") ~ pspline(t, df = 0, calc = TRUE) + survival::strata(c),
    weights = `P(C=c|y,t)`,
    data = .,
    dist = "gaussian",
    control = survreg.control(maxiter = maxiter_survreg, debug = verbose > 3))




ggplot() +
  geom_function(fun = function(t){predict(mm, newdata = data.frame(t = t, c = "1"))}, aes(color = "Component 1", linetype = "Fitted Model")) +
  geom_function(fun = function(t){predict(mm, newdata = data.frame(t = t,  c = "2"))}, aes(color = "Component 2", linetype = "Fitted Model"))
