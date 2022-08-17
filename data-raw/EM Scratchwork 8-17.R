rm(list = ls())
set.seed(1)
library(magrittr)
library(dplyr)
#<<<<<<< HEAD
load_all()


#NEXT STEPS:
##DIFFERENT SIGMAS
##INTERVAL CENSORING
##MORE THAN 2 COMPONENTS
##FUNCTIONS OF TIME AND COVARIATES: MEANS AND PI

#=======
library(mic.sim)
library(ggplot2)
library(LearnBayes)
library(survival)
library(biglm)
##Covariate inputs--------------------
covariate_effect_vector <- c(0 #0 at start is intercept, then add in the desired coefficients for the covariates
)

#a is the baseline level for all categorical covariates so, add terms for the coefficients for other levels of the variable that reflect the difference between that level and a

## y = B0 + B1X1 + B2X2b + B3X2c + B4X3b + B5X3c + B6X4b + B7X4c + B8X4d + B9X4e + B10X5 + \epsilon

covariate_list <-
  NULL


covariate_names <- NULL

#>>>>>>> f8a23b83e1c920f1303df831494854a95491ecc4

#data generation------------
n=2000

ncomp = 2

pi1 = function(t) {z <- 0.6
c("1" = z, "2" = 1- z)}

`E[X|T,C]` = function(t, c)
{
  case_when(
    c == "1" ~ 0 - 0.5 * t,
    c == "2" ~ 3 + 0.4 * t,
    TRUE ~ NaN
  )
}

t_dist1 = function(n){runif(n, min = 0, max = 4)}

sd_vector = c("1" = 1, "2" = 1)

low_con = 2^-4
high_con = 2^4


data.sim <- simulate_mics(
  n = n,
  t_dist = t_dist1,
  pi = pi1,
  `E[X|T,C]` = `E[X|T,C]`,
  sd_vector = sd_vector,
  covariate_list = covariate_list,
  covariate_effect_vector = covariate_effect_vector,
  low_con = low_con,
  high_con = high_con)


hist(data.sim$observed_value)

data.sim %>%
  ggplot() +
  geom_histogram(aes(x = observed_value, fill = factor(indicator)), binwidth = 1, boundary = 0, color = "black")


data.sim %>% group_by(comp) %>%
  summarise(
    n = n()) %>%
  mutate(total = n / sum(n))




visible_data <- data.sim %>%
  select(t, left_bound, right_bound, true_comp = comp) %>%
  mutate(obs_id = 1:n(),
         left_bound = log2(left_bound),
         right_bound = log2(right_bound)) %>%
  relocate(obs_id, .before = everything())

#pi_initial <- 0.5
#mu_initial <-
#sigma_initial <-
# random_complement_probs <- function(min = 0, max = 1){
#   a <- runif(1, min, max)
#   b <- 1-a
#   c(a, b)}
# random_complement_probs()
alpha <- rep(.1, ncomp)
posteriorP <- rdirichlet(length(n), alpha)



median_y <- median(visible_data$left_bound)
#first E step-----
possible_data <- visible_data %>% #visible data with c for component
  group_by_all() %>%
  summarise(
    c = as.character(1:2), #split at mean and assign
    `P(C=c|y,t)` = LearnBayes::rdirichlet(1, rep(.1, ncomp)) %>% as.vector(),
    .groups = "drop"
  ) %>%
   mutate(
   `P(C=c|y,t)` = case_when(left_bound > median_y & c == "1" ~ 0.6,
                            left_bound > median_y & c == "2" ~ 0.4,
                            left_bound <= median_y & c == "1" ~ 0.4,
                            left_bound <= median_y & c == "2" ~ 0.6)
   ) %>%
  print()


max_it = 3000

likelihood_documentation <- matrix(data = NA, nrow = max_it, ncol = 2)
likelihood_documentation [,1] <- 1:max_it



for(i in 1:max_it){
  message("starting iteration number ", i)
  #first M step--------
  #MLE of all parameters
  if(i != 1){
    oldmodel <- newmodel
    oldpi <- pi
    old_log_likelihood <- log_likelihood
    old_possible_data <- possible_data
  }

  #model P(Y|t,c)
  # model0 <- biglm(
  #   formula = y ~ c - 1,
  #   weights =  ~`P(C=c|y,t)`,
  #   data = possible_data) #if not from normal, change the link function and error dist
  #eg if lognormal, survreg with lognormal(change error distand link function)
  model <- survival::survreg(
    formula = Surv(time = left_bound,
                   time2 = right_bound,
                   type = "interval2") ~ c - 1,
    weights = `P(C=c|y,t)`,
    data = possible_data,
    dist = "gaussian")


  #Estimate mixing probabilities--------
  pi <- possible_data %>%
    group_by(c) %>%
    summarise(`P(C = c)` = sum(`P(C=c|y,t)`) / nrow(visible_data) )
  #pull(`P(C = c)`, name = c)

  # newmodel <- c(model$coefficients, sigma(model))
  `sd(y)` = model$scale
  newmodel = c(coef(model), `sd(y)`)

  if(i != 1){
    check_model = max(abs(newmodel - oldmodel))
    check_pi_tibble = tibble(c = 1:2, pi_dif = pi$`P(C = c)` - oldpi$`P(C = c)`)
    check_pi = max(abs(check_pi_tibble$pi_dif))


    param_checks = check_model < 0.00001 && check_pi < 0.00001


    #  if( check_model < 0.00001 && check_pi < 0.00001)
    #  {message("stopped on coefficients")
    #    break}
    #
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
           `Var[Y|t,c]` = `sd(y)`^2,
           `sd[Y|t,c]` = `sd(y)`,
           `P(Y|t,c)` =  if_else(
             left_bound == right_bound,
             dnorm(x = left_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`),
             pnorm(right_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`) - pnorm(left_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`)
           )
           ) %>%
    group_by(obs_id) %>%
    mutate(
      `P(c,y|t)` = `P(Y|t,c)` * `P(C = c)`,
      `P(Y=y|t)` = sum(`P(c,y|t)`),
      `P(C=c|y,t)` = `P(c,y|t)` / `P(Y=y|t)`
    ) %>% ungroup()
  print(pi)
  print(newmodel)

  log_likelihood_obs <- possible_data %>%
    group_by(obs_id) %>%
    summarise(likelihood_i = sum(`P(c,y|t)`)) %>%
    mutate(log_likelihood_i = log(likelihood_i))
  log_likelihood <- sum(log_likelihood_obs$log_likelihood_i)

  likelihood_documentation[i, 2] <- log_likelihood


  message(log_likelihood)

  plot(x = likelihood_documentation[1:i,1], y = likelihood_documentation[1:i,2], type = "l")

  if(i != 1){

    check_ll = log_likelihood - old_log_likelihood

    check_ll_2 = check_ll < 0.000000000001


    #if(check_ll < 0 ){
    #  warning("Log Likelihood decreased")  ###  HAS BEEN GETTING USED A LOT, WHY IS THE LOG LIKELIHOOD GOING DOWN????
    #}



    if(check_ll_2 && param_checks){
      message("Stopped on combined LL and parameters")
      break
    }

  }


}



#we need to calculate P(Y|t,c) and add to data set

plot(x = likelihood_documentation[1:i,1], y = likelihood_documentation[1:i,2], type = "l") ##Log likelihood appears to be decreasing???????

lm(observed_value ~ comp - 1, data = data.sim)

model
#missing from algorithm (aside from extension) is convergence criterion: likelihood and parameters we are estimating, see how much they change by and if they change by less than some amount you stop

data.sim %>% group_by(comp) %>%
  summarise(
    n = n()) %>%
  mutate(total = n / sum(n))

pi



sigma(data.sim_summary)

.391

.549

##Possible issue with weights?
##Main issue is that sigma starts correct, drops a few times (and the likelihood also drops), then sigma increases and likelihood starts to climb, but sigma does not get back to where it started



glm(y ~ c - 1, weights =  `P(C=c|y,t)`, data = possible_data)
sigma(glm(y ~ c - 1, weights =  `P(C=c|y,t)`, data = possible_data))


lm(y ~ c - 1, weights =  `P(C=c|y,t)`, data = old_possible_data)
sigma(lm(y ~ c - 1, weights =  `P(C=c|y,t)`, data = old_possible_data))


lm(y ~ c - 1, weights =  `P(C=c|y,t)`, data = old_possible_data)
sigma(lm(y ~ c - 1, weights =  `P(C=c|y,t)`, data = old_possible_data))

old_possible_data %>%
  mutate(`P(C=c|y,t)` = ifelse(`P(C=c|y,t)` < 0.00001, 0 , `P(C=c|y,t)`))


sum(old_possible_data[1:2,5, drop = TRUE])

possible_data %>% filter(c == 1) %>% ggplot2::ggplot(aes(x = t, y= y, color = `P(C=c|y,t)`)) + geom_point() + facet_wrap(~true_comp) + ggtitle("2")
old_possible_data %>% filter(c == 1) %>% ggplot2::ggplot(aes(x = t, y= y, color = `P(C=c|y,t)`)) + geom_point() + facet_wrap(~true_comp) + ggtitle("1")



