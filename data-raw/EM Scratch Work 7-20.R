library(magrittr)

#data generation------------
n=100

pi1 = function(t) {z <- 0.65
c("1" = z, "2" = 1- z)}

complist1 = list(
  "1" = function(t) {0*t},
  "2" = function(t) {3.5 + 0*t})

t_dist1 = function(n){runif(n, min = 0, max = 1)}

sd_vector = c("1" = 1, "2" = 1)

low_con = 2^-4
high_con = 2^4


data.sim <- simulate_mics(
  n = n,
  t_dist = t_dist1,
  pi = pi1,
  complist = complist1,
  sd_vector = sd_vector,
  covariate_list = covariate_list,
  covariate_effect_vector = covariate_effect_vector,
  low_con = low_con,
  high_con = high_con)


hist(data.sim$observed_value)


visible_data <- data.sim %>%
  select(c(t, y = observed_value)) %>%
  mutate(obs_id = 1:n()) %>%
  relocate(obs_id, .before = everything())

#pi_initial <- 0.5
#mu_initial <-
#sigma_initial <-
random_complement_probs <- function(min = 0, max = 1){
a <- runif(1, min, max)
b <- 1-a
c(a, b)}
random_complement_probs()

mean_y <- mean(visible_data$y)
#first E step-----
possible_data <- visible_data %>% #visible data with c for component
  group_by_all() %>%
  summarise(
    c = as.character(1:2), #split at mean and assign
    .groups = "drop"
  ) %>%
  mutate(
    `P(C=c|y,t)` = ifelse(y > mean_y, c == "1", c == "2"
                          ) %>% as.numeric()) %>%
  print()


max_it = 80


for(i in 1:max_it){
  message("starting iteration number ", i)
  #first M step--------
  #MLE of all parameters
  if(i != 1){
    oldmodel <- c(model$coefficients, sigma(model))
    oldpi <- pi}

  #model P(Y|t,c)
  model <- lm(y ~ c - 1, weights =  `P(C=c|y,t)`, data = possible_data) #if not from normal, change the link function and error dist
  #eg if lognormal, survreg with lognormal(change error distand link function)



  #Estimate mixing probabilities--------
  pi <- possible_data %>%
    group_by(c) %>%
    summarise(`P(C = c)` = sum(`P(C=c|y,t)`) / nrow(visible_data) )
  #pull(`P(C = c)`, name = c)

  if(i != 1){
    check_model = min((c(model$coefficients, sigma(model)) - oldmodel)^2)
    check_pi = tibble(c = 1:2, pi_dif = pi$`P(C = c)` - oldpi$`P(C = c)`)
  }
  #marginal_likelihood  #add warning: Chicago style pizza is a casserole
  #observed data likelihood sum(log(P(y_i|t_i)))
  #`P(Y|t,c)` * `P(C=c|t)` = P(Y, C|t) then sum over possible values of C
  #aka P(C = c)

  #group by obs_id then sum `P(c,y|t)` to get likelihood for each observation, take log of that, and then sum of those is observed data likelihood


  #once we have likelihood, make sure:
  #A. it is going up (with EM algorithm it should always increase)
  #B. if it is not going up by very much, you can stop
  #if conditions are met, use break



  #Next E step-------------
  possible_data %<>%
    select(-any_of("P(C = c)")) %>%
    left_join(pi, by = "c") %>%
    mutate(`E[Y|t,c]` = predict(model),
           `Var[Y|t,c]` = sigma(model)^2,
           `sd[Y|t,c]` = sigma(model),
           `P(Y|t,c)` = dnorm(y, mean = `E[Y|t,c]`, sd = `sd[Y|t,c]`)) %>%
    group_by(obs_id) %>%
    mutate(
      `P(c,y|t)` = `P(Y|t,c)` * `P(C = c)`,
      `P(Y=y|t)` = sum(`P(c,y|t)`),
      `P(C=c|y,t)` = `P(c,y|t)` / `P(Y=y|t)`
    ) %>% ungroup()
  print(pi)
}


#we need to calculate P(Y|t,c) and add to data set

lm(observed_value ~ comp - 1, data = data.sim)

#missing from algorithm (aside from extension) is convergence criterion: likelihood and parameters we are estimating, see how much they change by and if they change by less than some amount you stop


