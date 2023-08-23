library(magrittr)
library(dplyr)
library(tidyr)
#library(mic.sim)
library(LearnBayes)
library(survival)
library(gridExtra)
library(data.table)
library(purrr)
library(stringr)
library(gam)
load_all()


i = 100

pi = function(t) {
  m1 <- -1.5 + 0.1 * t
  z1 <- (1+ exp(-m1))^-1 #if exp(m) gets large, it won't add the 1 so we write like this
  m2 <- -1 - 0.008 * t
  z2 <- (1+ exp(-m2))^-1
  tibble("1" = 1 - z1 - z2, "2" = z1, "3" = z2)
}

c2 = function(t){
  m1 <- -1.5 + 0.1 * t
  z1 <- (1+ exp(-m1))^-1
  return(z1)
}

c3 = function(t){
  m2 <- -1 - 0.008 * t
  z2 <- (1+ exp(-m2))^-1
  return(z2)
}

c1 = function(t, c2, c3){
  a <- 1 - c2(t) - c3(t)
  return(a)
}


data.sim %>%
ggplot() +
  geom_function(fun = function(t){c1(t = t, c2 = c2, c3 = c3)}, aes(color = "comp1")) +
  geom_function(fun = function(t){c2(t = t)}, aes(color = "comp2")) +
  geom_function(fun = function(t){c3(t = t)}, aes(color = "comp3")) +
  xlim(0, 16) + ylim(0, 1)




`E[X|T,C]` = function(t, c)
{
  case_when(
    c == "1" ~ -4 + 0.01 * t,
    c == "2" ~ -1 + 0.02 * t,
    c == "3" ~ 2 - 0.02 * t,
    TRUE ~ NaN
  )
}

sd_vector = c("1" = 0.5, "2" = 0.5, "3" = 0.5)

t_dist = function(n){runif(n, min = 0, max = 16)}

covariate_effect_vector <- c(0) #0 at start is intercept, then add in the desired coefficients for the covariates
covariate_list <-  NULL
covariate_names <- NULL
n=300
ncomp = 3

attr(t_dist, "min") = 0
attr(t_dist, "max") = 16

 #0.5, 0.75, 1, 1.25

low_con = -4
high_con = 2 #errored out when this was 2^3
#RUN 1 : 2
#RUN 2: 3
#RUN 3: 4

scale = "log"

formula = Surv(time = left_bound,
               time2 = right_bound,
               type = "interval2") ~ pspline(t, df = 0, calc = TRUE)

formula2 = c == "2" ~ s(t)
formula3 = list(cc ~ s(t), ~ s(t))
max_it = 3000
ncomp = 3
tol_ll = 1e-6
maxiter_survreg = 30
pi_function = TRUE
pi_link = "logit"
verbose = 2
allow_safety = TRUE
cutoff = 0.9
fms_only = FALSE
initial_weighting = 1
keep_true_values = TRUE
conc_limits_table = NULL
max_cens_tolerance = 0.8

set.seed(i)

simulate_mics(n = n,
              t_dist = t_dist,
              pi = pi,
              `E[X|T,C]` = `E[X|T,C]`,
              sd_vector = sd_vector,
              covariate_list = covariate_list,
              covariate_effect_vector = covariate_effect_vector,
              conc_limits_table = conc_limits_table,
              low_con = low_con,
              high_con = high_con,
              scale = scale) %>%
            prep_sim_data_for_em(
              .,
              left_bound_name = "left_bound",
              right_bound_name = "right_bound",
              time = "t",
              covariate_names = covariate_names,
              scale = scale,
              keep_truth = keep_true_values,
              observed_value_name = "observed_value",
              comp_name = "comp"
            ) -> visible_data

visible_data %>% summarise(.by = comp, n = n())

output <- fit_model_pi_3(
    visible_data,

    formula = Surv(time = left_bound,
                   time2 = right_bound,
                   type = "interval2") ~ pspline(t, df = 0, calc = TRUE),
    formula3 = list(cc ~ s(t), ~ s(t)), #or: c == "2" ~ lo(t)
    max_it = 3000,
    ncomp = 2,
    tol_ll = 1e-6,
    browse_at_end = FALSE,
    browse_each_step = FALSE,
    plot_visuals = FALSE,
    pi_link = "logit",
    #silent = FALSE,
    verbose = 3,
    #low_con = 2^-3,
    #high_con = 2^3,
    maxiter_survreg = 30,
    initial_weighting = 1 #smoothingspline or loess
)


output$newmodel[[3]]



df_output = output$possible_data %>% mutate(cens =
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
                                       ))

if(nrow(df_output %>% filter(left_bound == -Inf)) > 0){
  plot_min <- (df_output %>% filter(left_bound == -Inf) %>% pull(right_bound) %>% min) - 1
}else{
  plot_min <- (df_output %>% pull(left_bound) %>% min) - 1
}

if(nrow(df_output %>% filter(right_bound == Inf)) > 0){
  plot_max <- (df_output %>% filter(right_bound == Inf) %>% pull(left_bound) %>% max) + 1
}else{
  plot_max <- (df_output %>% pull(right_bound) %>% max) + 1
}

mu.se.brd <- function(t, c, z){predict(output$newmodel[[c]], data.frame(t = t)) + z * predict(output$newmodel[[c]], data.frame(t = t), se = TRUE)$se.fit}
mu.se.brd.fms <- function(t, z){predict(output$newmodel, data.frame(t = t)) + z * predict(output$newmodel, data.frame(t = t), se = TRUE)$se.fit}



output$newmodel[[1]]$scale %>% print
output$newmodel[[2]]$scale %>% print
output$newmodel[[3]]$scale %>% print

df_output %>% group_by(obs_id) %>% top_n(1, `P(C=c|y,t)`) %>%
 ggplot() +
  #geom_bar(aes(x = mid, fill = cens)) +
 # geom_point(aes(x = t, y = mid, color = `P(C=c|y,t)`), data = df, alpha = 0) +
  geom_segment(aes(x = t, xend = t, y = left_bound, yend = right_bound, color = comp), data = (. %>% filter(cens == "int")), alpha = 0.2) +
  geom_segment(aes(x = t, xend = t, y = right_bound, yend = left_bound, color = comp), data = (. %>% filter(cens == "lc") %>% mutate(left_bound = right_bound - 1.5)), arrow = arrow(length = unit(0.03, "npc")), alpha = 0.2) +
  geom_segment(aes(x = t, xend = t, y = left_bound, yend = right_bound, color = comp), data = (. %>% filter(cens == "rc") %>% mutate(right_bound = left_bound + 1.5)), arrow = arrow(length = unit(0.03, "npc")), alpha = 0.2) +
  geom_point(aes(x = t, y = left_bound), data = . %>% filter(left_bound != -Inf)) +
  geom_point(aes(x = t, y = right_bound), data = . %>% filter(right_bound != Inf)) +
 # scale_colour_gradientn(colours = c("purple", "orange")) +
  #ylim(plot_min - 0.5, plot_max + 0.5) +
  ggtitle(drug) +
  xlab("Time") +
  ylab("MIC") +
 # ggnewscale::new_scale_color() +
  geom_function(fun = function(t){predict(output$newmodel[[1]], newdata = data.frame(t = t))}, aes(color = "Component 1 Mu", linetype = "Fitted Model")) +
  geom_function(fun = function(t){predict(output$newmodel[[2]], newdata = data.frame(t = t))}, aes(color = "Component 2 Mu", linetype = "Fitted Model")) +
  geom_function(fun = function(t){predict(output$newmodel[[3]], newdata = data.frame(t = t))}, aes(color = "Component 3 Mu", linetype = "Fitted Model")) +
  geom_function(fun = function(t){mu.se.brd(t, c = 1, z = 1.96)}, aes(color = "Component 1 Mu", linetype = "Fitted Model SE"), size = 0.6, alpha = 0.6) +
  geom_function(fun = function(t){mu.se.brd(t, c = 1, z = -1.96)}, aes(color = "Component 1 Mu", linetype = "Fitted Model SE"), size = 0.6, alpha = 0.6) +
  geom_function(fun = function(t){mu.se.brd(t, c = 2, z = 1.96)}, aes(color = "Component 2 Mu", linetype = "Fitted Model SE"), size = 0.6, alpha = 0.6) +
  geom_function(fun = function(t){mu.se.brd(t, c = 2, z = -1.96)}, aes(color = "Component 2 Mu", linetype = "Fitted Model SE"), size = 0.6, alpha = 0.6) +
  geom_function(fun = function(t){mu.se.brd(t, c = 3, z = 1.96)}, aes(color = "Component 3 Mu", linetype = "Fitted Model SE"), size = 0.6, alpha = 0.6) +
  geom_function(fun = function(t){mu.se.brd(t, c = 3, z = -1.96)}, aes(color = "Component 3 Mu", linetype = "Fitted Model SE"), size = 0.6, alpha = 0.6)



