#
# data_tibble <- tibble(my_var = c(650, 1040, 1060, 1150, 1180, 1220, 1280, 1430, 1440, 1440, 1470, 1470, 1480, 1490, 1520, 1550, 1560, 1560, 1600, 1600, 1610, 1630, 1660, 1740, 1780, 1800, 1810, 1820, 1830, 1870, 1910, 1910, 1930, 1940, 1940, 1940, 1980, 1990, 2000, 2060, 2080, 2080, 2090, 2100, 2120, 2140, 2160, 2240, 2260, 2320, 2430, 2440, 2540, 2550, 2560, 2570, 2610, 2660, 2680, 2700, 2700, 2720, 2730, 2790, 2820, 2880, 2910, 2970, 2970, 3030, 3050, 3060, 3080, 3120, 3160, 3200, 3280, 3290, 3310, 3320, 3340, 3350, 3400, 3430, 3540, 3550, 3580, 3580, 3620, 3640, 3650, 3710, 3820, 3820, 3870, 3980, 4060, 4070, 4160, 4170, 4170, 4220, 4300, 4320, 4350, 4390, 4430, 4450, 4500, 4650, 4650, 5080, 5160, 5160, 5460, 5490, 5670, 5680, 5760, 5960, 5980, 6060, 6120, 6190, 6480, 6760, 7750, 8390, 9560))
#
# stat_layers <- tibble(distribution = c("lognormal", "gamma", "normal"),
#                       fun = c(dlnorm, dgamma, dnorm),
#                       colour = c("red", "green", "yellow")) %>%
#   mutate(args = map(distribution, MASS::fitdistr, x=data_tibble$my_var)) %>%
#   mutate(args = map(args, ~as.list(.$estimate))) %>%
#   select(-distribution) %>%
#   pmap(stat_function)
#
# ?pmap
#
#
# x <- list(1, 1, 1)
# y <- list(10, 20, 30)
# z <- list(100, 200, 300)
#
# pmap(list(x, y, z), sum)
#
#
# data.table::rbindlist(list(tibble(a = c(dnorm), b = c(cnorm)),
# tibble(a = c(dnorm), b = c(cnorm)))) %>% tibble

##new plan:
    #make each row of tibble, where i, j, mutate in set cut down to the correct one and just the models
#loop over each of 12 sets

setwd("~/Desktop/Dissertation Project/Chapter 1/simulation_scripts")

library(purrr)
library(ggplot2)

setup_id = 1
ymin = -5
ymax = 10
low_con = -5
high_con = 3

`E[X|T,C]` = list(function(t, c){
  case_when(
    c == "1" ~ -3.0 + 0.2 * t,
    c == "2" ~ -1 + (15 * sqrt((t ^ 0.7) * 0.02)),
    TRUE ~ NaN
  )
})

sd_vector = list(c("1" = 1, "2" = 1))

sd_vals = list(
  0.1,
  0.2,
  0.3,
  0.4
)

pi_vals = list(
  function(t) {
    z <- 0 + (0.002 * t)
    #z <- (1+ exp(-m))^-1 #if exp(m) gets large, it won't add the 1 so we write like this
    tibble("1" = 1 - z, "2" = z)
  },
  function(t) {
    z <- 0.1 + (0.002 * t)
    #z <- (1+ exp(-m))^-1 #if exp(m) gets large, it won't add the 1 so we write like this
    tibble("1" = 1 - z, "2" = z)
  },
  function(t) {
    z <- 0.2 + (0.002 * t)
    #z <- (1+ exp(-m))^-1 #if exp(m) gets large, it won't add the 1 so we write like this
    tibble("1" = 1 - z, "2" = z)
  }
)



param_grid = tidyr::expand_grid(sd_vals = sd_vals, pi_vals = pi_vals, `E[X|T,C]` = `E[X|T,C]`, sd_vector = sd_vector) %>%
  mutate(description =
           c(
             "SD Initial Param = 0.1, Prop C2 = 0 + (0.002 * t)",
             "SD Initial Param = 0.1, Prop C2 = 0.1 + (0.002 * t)",
             "SD Initial Param = 0.1, Prop C2 = 0.2 + (0.002 * t)",
             "SD Initial Param = 0.2, Prop C2 = 0 + (0.002 * t)",
             "SD Initial Param = 0.2, Prop C2 = 0.1 + (0.002 * t)",
             "SD Initial Param = 0.2, Prop C2 = 0.2 + (0.002 * t)",
             "SD Initial Param = 0.3, Prop C2 = 0 + (0.002 * t)",
             "SD Initial Param = 0.3, Prop C2 = 0.1 + (0.002 * t)",
             "SD Initial Param = 0.3, Prop C2 = 0.2 + (0.002 * t)",
             "SD Initial Param = 0.4, Prop C2 = 0 + (0.002 * t)",
             "SD Initial Param = 0.4, Prop C2 = 0.1 + (0.002 * t)",
             "SD Initial Param = 0.4, Prop C2 = 0.2 + (0.002 * t)"
           ))

t1 = function(i, j, set){
  comp = 1
  current_set = set[[i]][[j]]

  if(length(current_set$models) == 3){
    high = current_set$models[[1]]$output
    med = current_set$models[[2]]$output
    low = current_set$models[[3]]$output
  }else if(length(current_set$models) == 2){
    high = current_set$models[[1]]$output
    med = NULL
    low = current_set$models[[2]]$output
  }else if(length(current_set$models) == 1){
    high = current_set$models[[1]]$output
    med = NULL
    low = NULL
  }else{
    errorCondition("Should only grab 3 example models, not more")
  }


  fun =
    function(t, high, comp){
      if(!is.null(high)) {
        if (!is.na(high$mu_model[[comp]]$scale) &
            !is.na(predict(high$mu_model[[comp]], newdata = data.frame(t = 2)))) {

          predict(high$mu_model[[comp]], newdata = data.frame(t = t))
        }else{
          return(NA_integer_)
        }
      }else{
        return(NA_integer_)
      }
    }





  return(tibble(
    i = i,
    j = j,
    colour = (2 * i) - (2 - j),
    args = list(list(high = high,
                     comp = comp)),
    fun = c(fun)
  ) )
}
t2 = function(i, j, set){
  comp = 2
  current_set = set[[i]][[j]]

  if(length(current_set$models) == 3){
    high = current_set$models[[1]]$output
    med = current_set$models[[2]]$output
    low = current_set$models[[3]]$output
  }else if(length(current_set$models) == 2){
    high = current_set$models[[1]]$output
    med = NULL
    low = current_set$models[[2]]$output
  }else if(length(current_set$models) == 1){
    high = current_set$models[[1]]$output
    med = NULL
    low = NULL
  }else{
    errorCondition("Should only grab 3 example models, not more")
  }


  fun =
    function(t, high, comp){
      if(!is.null(high)) {
        if (!is.na(high$mu_model[[comp]]$scale) &
            !is.na(predict(high$mu_model[[comp]], newdata = data.frame(t = 2)))) {

          predict(high$mu_model[[comp]], newdata = data.frame(t = t))
        }else{
          return(NA_integer_)
        }
      }else{
        return(NA_integer_)
      }
    }





  return(tibble(
    i = i,
    j = j,
    colour = (2 * i) - (2 - j),
    args = list(list(high = high,
                     comp = comp)),
    fun = c(fun)
  ) )
}
t3 = function(i, j, set){
  comp = 1
  current_set = set[[i]][[j]]

  if(length(current_set$models) == 3){
    high = current_set$models[[1]]$output
    med = current_set$models[[2]]$output
    low = current_set$models[[3]]$output
  }else if(length(current_set$models) == 2){
    high = current_set$models[[1]]$output
    med = NULL
    low = current_set$models[[2]]$output
  }else if(length(current_set$models) == 1){
    high = current_set$models[[1]]$output
    med = NULL
    low = NULL
  }else{
    errorCondition("Should only grab 3 example models, not more")
  }


  fun =
    function(t, med, comp){
      if(!is.null(med)) {
        if (!is.na(med$mu_model[[comp]]$scale) &
            !is.na(predict(med$mu_model[[comp]], newdata = data.frame(t = 2)))) {

          predict(med$mu_model[[comp]], newdata = data.frame(t = t))
        }else{
          return(NA_integer_)
        }
      }else{
        return(NA_integer_)
      }

    }




  return(tibble(
    i = i,
    j = j,
    colour = (2 * i) - (2 - j),
    args = list(list(med = med,
                     comp = comp)),
    fun = c(fun)
  ) )
}
t4 = function(i, j, set){
  comp = 2
  current_set = set[[i]][[j]]

  if(length(current_set$models) == 3){
    high = current_set$models[[1]]$output
    med = current_set$models[[2]]$output
    low = current_set$models[[3]]$output
  }else if(length(current_set$models) == 2){
    high = current_set$models[[1]]$output
    med = NULL
    low = current_set$models[[2]]$output
  }else if(length(current_set$models) == 1){
    high = current_set$models[[1]]$output
    med = NULL
    low = NULL
  }else{
    errorCondition("Should only grab 3 example models, not more")
  }


  fun =
    function(t, med, comp){
      if(!is.null(med)) {
        if (!is.na(med$mu_model[[comp]]$scale) &
            !is.na(predict(med$mu_model[[comp]], newdata = data.frame(t = 2)))) {

          predict(med$mu_model[[comp]], newdata = data.frame(t = t))
        }else{
          return(NA_integer_)
        }
      }else{
        return(NA_integer_)
      }
    }





  return(tibble(
    i = i,
    j = j,
    colour = (2 * i) - (2 - j),
    args = list(list(med = med,
                     comp = comp)),
    fun = c(fun)
  ) )
}
t5 = function(i, j, set){
  comp = 1
  current_set = set[[i]][[j]]

  if(length(current_set$models) == 3){
    high = current_set$models[[1]]$output
    med = current_set$models[[2]]$output
    low = current_set$models[[3]]$output
  }else if(length(current_set$models) == 2){
    high = current_set$models[[1]]$output
    med = NULL
    low = current_set$models[[2]]$output
  }else if(length(current_set$models) == 1){
    high = current_set$models[[1]]$output
    med = NULL
    low = NULL
  }else{
    errorCondition("Should only grab 3 example models, not more")
  }


  fun =
    function(t, low, comp){
      if(!is.null(low)) {
        if (!is.na(low$mu_model[[comp]]$scale) &
            !is.na(predict(low$mu_model[[comp]], newdata = data.frame(t = 2)))) {

          predict(low$mu_model[[comp]], newdata = data.frame(t = t))
        }else{
          return(NA_integer_)
        }
      }else{
        return(NA_integer_)
      }

    }




  return(tibble(
    i = i,
    j = j,
    colour = (2 * i) - (2 - j),
    args = list(list(low = low,
                     comp = comp)),
    fun = c(fun)
  ) )
}
t6 = function(i, j, set){
  comp = 2
  current_set = set[[i]][[j]]

  if(length(current_set$models) == 3){
    high = current_set$models[[1]]$output
    med = current_set$models[[2]]$output
    low = current_set$models[[3]]$output
  }else if(length(current_set$models) == 2){
    high = current_set$models[[1]]$output
    med = NULL
    low = current_set$models[[2]]$output
  }else if(length(current_set$models) == 1){
    high = current_set$models[[1]]$output
    med = NULL
    low = NULL
  }else{
    errorCondition("Should only grab 3 example models, not more")
  }


  fun =
    function(t, low, comp){
      if(!is.null(low)) {
        if (!is.na(low$mu_model[[comp]]$scale) &
            !is.na(predict(low$mu_model[[comp]], newdata = data.frame(t = 2)))) {

          predict(low$mu_model[[comp]], newdata = data.frame(t = t))
        }else{
          return(NA_integer_)
        }
      }else{
        return(NA_integer_)
      }
    }





  return(tibble(
    i = i,
    j = j,
    colour = (2 * i) - (2 - j),
    args = list(list(low = low,
                     comp = comp)),
    fun = c(fun)
  ) )
}



plot_sim_results = function(row, param_grid, setup_id, return = "one_row_splits"){


  set = loadRData(paste0(getwd(), "/setup_", setup_id, "_analysis_set_", row, ".Rdata"))
  param_grid_one_row = param_grid[row,]

  pi = (param_grid_one_row$pi_vals)[[1]]

  `E[X|T,C]` = (param_grid_one_row$`E[X|T,C]`)[[1]]



#set



if(return == "one_row_splits"){
high_comp_1_layers = map2_df(rep(1:10, each = 4), rep(rep(1:2, 10), each = 2), ~t1(.x, .y, set = set)) %>% select(-c(i, j)) %>% mutate(colour = as.factor(colour)) %>% select(fun, colour, args) %>%
  pmap(stat_function)

high_comp_2_layers = map2_df(rep(1:10, each = 4), rep(rep(1:2, 10), each = 2), ~t2(.x, .y, set = set)) %>% select(-c(i, j)) %>% mutate(colour = as.factor(colour)) %>% select(fun, colour, args) %>%
  pmap(stat_function)
H_mu = ggplot() + high_comp_1_layers + high_comp_2_layers + xlim(0, 16) + ylim(ymin, ymax) +  ggtitle(paste0(param_grid_one_row %>% pull(description),": High Likelihood: Mu")) +
  geom_hline(colour = "orange", aes(linetype = "low_con", yintercept = low_con), size = 1) +
  geom_hline(colour = "orange", aes(linetype = "high_con", yintercept = high_con), size = 1) +
  geom_function(fun = function(t){`E[X|T,C]`(t, c = "1")}, color = "black", size = 1, aes(linetype = "truth")) +
  geom_function(fun = function(t){`E[X|T,C]`(t, c = "2")}, color = "black", size = 1, aes(linetype = "truth")) +
  scale_linetype_manual(c("low_con", "high_con", "truth"), values = c("dotted", "dotted", "dashed")) +
  guides(linetype = "none")  +
  theme(plot.title = element_text(size = 8))

med_comp_1_layers = map2_df(rep(1:10, each = 4), rep(rep(1:2, 10), each = 2), ~t3(.x, .y, set = set)) %>% select(-c(i, j)) %>% mutate(colour = as.factor(colour)) %>% select(fun, colour, args) %>%
  pmap(stat_function)

med_comp_2_layers = map2_df(rep(1:10, each = 4), rep(rep(1:2, 10), each = 2), ~t4(.x, .y, set = set)) %>% select(-c(i, j)) %>% mutate(colour = as.factor(colour)) %>% select(fun, colour, args) %>%
  pmap(stat_function)
M_mu = ggplot() + med_comp_1_layers + med_comp_2_layers + xlim(0, 16) + ylim(ymin, ymax) +  ggtitle(paste0(param_grid_one_row %>% pull(description), ": Med Likelihood: Mu")) +
  geom_hline(colour = "orange", aes(linetype = "low_con", yintercept = low_con), size = 1) +
  geom_hline(colour = "orange", aes(linetype = "high_con", yintercept = high_con), size = 1) +
  geom_function(fun = function(t){`E[X|T,C]`(t, c = "1")}, color = "black", size = 1, aes(linetype = "truth")) +
  geom_function(fun = function(t){`E[X|T,C]`(t, c = "2")}, color = "black", size = 1, aes(linetype = "truth")) +
  scale_linetype_manual(c("low_con", "high_con", "truth"), values = c("dotted", "dotted", "dashed")) +
  guides(linetype = "none")  +
  theme(plot.title = element_text(size = 8))


low_comp_1_layers = map2_df(rep(1:10, each = 4), rep(rep(1:2, 10), each = 2), ~t5(.x, .y, set = set)) %>% select(-c(i, j)) %>% mutate(colour = as.factor(colour)) %>% select(fun, colour, args) %>%
  pmap(stat_function)

low_comp_2_layers = map2_df(rep(1:10, each = 4), rep(rep(1:2, 10), each = 2), ~t6(.x, .y, set = set)) %>% select(-c(i, j)) %>% mutate(colour = as.factor(colour)) %>% select(fun, colour, args) %>%
  pmap(stat_function)
L_mu = ggplot() + low_comp_1_layers + low_comp_2_layers + xlim(0, 16) + ylim(ymin, ymax) +  ggtitle(paste0(param_grid_one_row %>% pull(description), ": Low Likelihood: Mu"))  +
  geom_hline(colour = "orange", aes(linetype = "low_con", yintercept = low_con), size = 1) +
  geom_hline(colour = "orange", aes(linetype = "high_con", yintercept = high_con), size = 1) +
  geom_function(fun = function(t){`E[X|T,C]`(t, c = "1")}, color = "black", size = 1, aes(linetype = "truth")) +
  geom_function(fun = function(t){`E[X|T,C]`(t, c = "2")}, color = "black", size = 1, aes(linetype = "truth")) +
  scale_linetype_manual(c("low_con", "high_con", "truth"), values = c("dotted", "dotted", "dashed")) +
  guides(linetype = "none") +
  theme(plot.title = element_text(size = 8))

}else{


high_comp_1_layers = map2_df(rep(1:10, each = 2), rep(rep(1:2, 10)), ~t1(.x, .y, set = set)) %>% select(-c(i, j)) %>% select(fun, args) %>% mutate(mapping = list(aes(alpha = 0.2, color = "high"))) %>%
  pmap(stat_function)

high_comp_2_layers = map2_df(rep(1:10, each = 2), rep(rep(1:2, 10)), ~t2(.x, .y, set = set)) %>% select(-c(i, j)) %>% select(fun, args) %>% mutate(mapping = list(aes(alpha = 0.2, color = "high"))) %>%
  pmap(stat_function)

med_comp_1_layers = map2_df(rep(1:10, each = 2), rep(rep(1:2, 10)), ~t3(.x, .y, set = set)) %>% select(-c(i, j)) %>% select(fun, args) %>% mutate(mapping = list(aes(alpha = 0.2, color = "med"))) %>%
  pmap(stat_function)

med_comp_2_layers = map2_df(rep(1:10, each = 2), rep(rep(1:2, 10)), ~t4(.x, .y, set = set)) %>% select(-c(i, j)) %>% select(fun, args) %>% mutate(mapping = list(aes(alpha = 0.2, color = "med"))) %>%
  pmap(stat_function)

low_comp_1_layers = map2_df(rep(1:10, each = 2), rep(rep(1:2, 10)), ~t5(.x, .y, set = set)) %>% select(-c(i, j)) %>% select(fun, args) %>% mutate(mapping = list(aes(alpha = 0.2, color = "low"))) %>%
  pmap(stat_function)

low_comp_2_layers = map2_df(rep(1:10, each = 2), rep(rep(1:2, 10)), ~t6(.x, .y, set = set)) %>% select(-c(i, j)) %>% select(fun, args) %>% mutate(mapping = list(aes(alpha = 0.2, color = "low"))) %>%
  pmap(stat_function)

data_1 = tibble(m = 1, weight = 0.01) %>%
ggplot() +
  low_comp_1_layers + low_comp_2_layers +
  med_comp_1_layers + med_comp_2_layers +
  high_comp_1_layers + high_comp_2_layers +
  geom_function(fun = function(t){`E[X|T,C]`(t, c = "1")}, aes(color = "truth")) +
  geom_function(fun = function(t){`E[X|T,C]`(t, c = "2")}, aes(color = "truth")) +
  xlim(0, 16) + ylim(ymin, ymax) +  ggtitle(paste0(param_grid_one_row %>% pull(description), ": Mu for High, Median, and Low Likelihood")) +
  geom_hline(yintercept = low_con, colour = "orange", linetype = "dashed") +
  geom_hline(yintercept = high_con, colour = "orange", linetype = "dashed") +
  scale_color_manual(c("high", "med","low","truth"), values = c("#F8766D", "#00BFC4", "#7CAE00", "black")) +
  guides(alpha = "none")  +
  theme(plot.title = element_text(size = 7))

#data_1

}

pi_analysis_sims = function(i, j, set, choose){

  current_set = set[[i]][[j]]

  if(length(current_set$models) == 3){
    high = current_set$models[[1]]$output
    med = current_set$models[[2]]$output
    low = current_set$models[[3]]$output
  }else if(length(current_set$models) == 2){
    high = current_set$models[[1]]$output
    med = NULL
    low = current_set$models[[2]]$output
  }else if(length(current_set$models) == 1){
    high = current_set$models[[1]]$output
    med = NULL
    low = NULL
  }else{
    errorCondition("Should only grab 3 example models, not more")
  }

  if(choose == "high"){
  fun =
    function(t, high){
      if(!is.null(high)){

          predict(high$pi_model, newdata = data.frame(t = t), type = "response")
      }else{
        return(NA_integer_)
      }
      }

  return(tibble(
    i = i,
    j = j,
    colour = (2 * i) - (2 - j),
    args = list(list(high = high)),
    fun = c(fun)
  ) )
  }else if(choose == "med"){
    fun =
      function(t, med){
        if(!is.null(med)){

          predict(med$pi_model, newdata = data.frame(t = t), type = "response")
        }else{
          return(NA_integer_)
        }
      }
  return(tibble(
    i = i,
    j = j,
    colour = (2 * i) - (2 - j),
    args = list(list(med = med)),
    fun = c(fun)
  ) )
  }else if(choose == "low"){

    fun =
      function(t, low){
        if(!is.null(low)){

          predict(low$pi_model, newdata = data.frame(t = t), type = "response")
        }else{
          return(NA_integer_)
        }
      }
  return(tibble(
    i = i,
    j = j,
    colour = (2 * i) - (2 - j),
    args = list(list(low = low)),
    fun = c(fun)
  ) )
  }else{
    errorCondition("low, med, or high")
  }

}

if(return == "one_row_splits"){
high_pi_layers = map2_df(rep(1:10, each = 2), rep(rep(1:2, 10)), ~pi_analysis_sims(.x, .y, set = set, choose = "high")) %>% select(-c(i, j)) %>% mutate(colour = as.factor(colour)) %>% select(fun, colour, args) %>%
  pmap(stat_function)
H_pi = ggplot() + high_pi_layers + xlim(0, 16) + ylim(0, 1) + ggtitle(paste0(param_grid_one_row %>% pull(description), ": High Likelihood: Pi")) +
  geom_function(fun = function(t){pi(t) %>% pull("2")}, color = "black", size = 1, linetype = "dashed")  +
  theme(plot.title = element_text(size = 8))

med_pi_layers = map2_df(rep(1:10, each = 2), rep(rep(1:2, 10)), ~pi_analysis_sims(.x, .y, set = set, choose = "med")) %>% select(-c(i, j)) %>% mutate(colour = as.factor(colour)) %>% select(fun, colour, args) %>%
  pmap(stat_function)
M_pi = ggplot() + med_pi_layers + xlim(0, 16) + ylim(0, 1) + ggtitle(paste0(param_grid_one_row %>% pull(description), ": Med Likelihood: Pi")) +
  geom_function(fun = function(t){pi(t) %>% pull("2")}, color = "black", size = 1, linetype = "dashed")  +
  theme(plot.title = element_text(size = 8))
low_pi_layers = map2_df(rep(1:10, each = 2), rep(rep(1:2, 10)), ~pi_analysis_sims(.x, .y, set = set, choose = "low")) %>% select(-c(i, j)) %>% mutate(colour = as.factor(colour)) %>% select(fun, colour, args) %>%
  pmap(stat_function)
L_pi = ggplot() + low_pi_layers + xlim(0, 16) + ylim(0, 1) + ggtitle(paste0(param_grid_one_row %>% pull(description), ": Low Likelihood: Pi")) +
  geom_function(fun = function(t){pi(t) %>% pull("2")}, color = "black", size = 1, linetype = "dashed")  +
  theme(plot.title = element_text(size = 8))

}else{


high_pi_layers = map2_df(rep(1:10, each = 2), rep(rep(1:2, 10)), ~pi_analysis_sims(.x, .y, set = set, choose = "high")) %>% select(-c(i, j)) %>% select(fun, args) %>% mutate(mapping = list(aes(alpha = 0.2, color = "high"))) %>%
  pmap(stat_function)
med_pi_layers = map2_df(rep(1:10, each = 2), rep(rep(1:2, 10)), ~pi_analysis_sims(.x, .y, set = set, choose = "med")) %>% select(-c(i, j)) %>% select(fun, args) %>% mutate(mapping = list(aes(alpha = 0.2, color = "med"))) %>%
  pmap(stat_function)
low_pi_layers = map2_df(rep(1:10, each = 2), rep(rep(1:2, 10)), ~pi_analysis_sims(.x, .y, set = set, choose = "low")) %>% select(-c(i, j)) %>% select(fun, args) %>% mutate(mapping = list(aes(alpha = 0.2, color = "low"))) %>%
  pmap(stat_function)

data_2 = tibble(m = 1, weight = 0.01) %>%
  ggplot() +
  low_pi_layers +
  med_pi_layers +
  high_pi_layers +
  xlim(0,16) + ylim(0,1) +
  geom_function(fun = function(t){pi(t) %>% pull("2")}, aes(color = "truth")) +  ggtitle(paste0(param_grid_one_row %>% pull(description), ": Pi for High, Median, and Low Likelihood")) +
  scale_color_manual(c("high", "med","low","truth"), values = c("#F8766D", "#00BFC4", "#7CAE00", "black")) +
  guides(alpha = "none") +
  theme(plot.title = element_text(size = 7))

}
#data_1/data_2



if(return == "one_row_splits"){
  return(((H_mu|M_mu|L_mu)/(H_pi|M_pi|L_pi)))
}else{
  return((data_1/data_2))
}

}

#likelihood_splits = map(1:12, ~ plot_sim_results(.x, param_grid = param_grid, setup_id = setup_id, return = "one_row_splits"))
#likelihoods_together = map(1:12, ~ plot_sim_results(.x, param_grid = param_grid, setup_id = setup_id, return = "likelihoods_together"))

name = as.character(paste0("setup_", setup_id,"_likelihood_splits_visuals.pdf"))

pdf(width = 11, height = 8.5, file = name)

    plot_sim_results(1, param_grid = param_grid, setup_id = setup_id, return = "one_row_splits")
    plot_sim_results(2, param_grid = param_grid, setup_id = setup_id, return = "one_row_splits")
    plot_sim_results(3, param_grid = param_grid, setup_id = setup_id, return = "one_row_splits")
    plot_sim_results(4, param_grid = param_grid, setup_id = setup_id, return = "one_row_splits")
    plot_sim_results(5, param_grid = param_grid, setup_id = setup_id, return = "one_row_splits")
    plot_sim_results(6, param_grid = param_grid, setup_id = setup_id, return = "one_row_splits")
    plot_sim_results(7, param_grid = param_grid, setup_id = setup_id, return = "one_row_splits")
    plot_sim_results(8, param_grid = param_grid, setup_id = setup_id, return = "one_row_splits")
    plot_sim_results(9, param_grid = param_grid, setup_id = setup_id, return = "one_row_splits")
    plot_sim_results(10, param_grid = param_grid, setup_id = setup_id, return = "one_row_splits")
    plot_sim_results(11, param_grid = param_grid, setup_id = setup_id, return = "one_row_splits")
    plot_sim_results(12, param_grid = param_grid, setup_id = setup_id, return = "one_row_splits")

dev.off()

name = as.character(paste0("setup_", setup_id,"_likelihood_combined_visuals.pdf"))

pdf(width = 11, height = 8.5, file = name)
if(nrow(param_grid) == 12){
  a = plot_sim_results(1, param_grid = param_grid, setup_id = setup_id, return = "combined")
  b = plot_sim_results(2, param_grid = param_grid, setup_id = setup_id, return = "combined")
  c = plot_sim_results(3, param_grid = param_grid, setup_id = setup_id, return = "combined")
  d = plot_sim_results(4, param_grid = param_grid, setup_id = setup_id, return = "combined")
  e = plot_sim_results(5, param_grid = param_grid, setup_id = setup_id, return = "combined")
  f = plot_sim_results(6, param_grid = param_grid, setup_id = setup_id, return = "combined")
  g = plot_sim_results(7, param_grid = param_grid, setup_id = setup_id, return = "combined")
  h = plot_sim_results(8, param_grid = param_grid, setup_id = setup_id, return = "combined")
  i = plot_sim_results(9, param_grid = param_grid, setup_id = setup_id, return = "combined")
  j = plot_sim_results(10, param_grid = param_grid, setup_id = setup_id, return = "combined")
  k = plot_sim_results(11, param_grid = param_grid, setup_id = setup_id, return = "combined")
  l = plot_sim_results(12, param_grid = param_grid, setup_id = setup_id, return = "combined")

 print((a|b|c)/(d|e|f))
  print((g|h|i)/(j|k|l))
  }
dev.off()




setwd("/Users/alecmichael/Desktop/Dissertation Project/Chapter 1/mic.sim")


