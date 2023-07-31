covariate_effect_vector <- c(0) #0 at start is intercept, then add in the desired coefficients for the covariates
covariate_list <-  NULL
covariate_names <- NULL
n=280
ncomp = 2
#pi_truth = "identity"



ggplot()+
  geom_function(fun = function(t){0.3 + 0.02 * t - 0.0005 * t^2}) +
  xlim(-0.5, 16.5) #+ ylim(0, 1)

pi = function(t) {
  z <- 0.15
#  z <- (1+ exp(-m))^-1 #if exp(m) gets large, it won't add the 1 so we write like this
  tibble("1" = 1 - z, "2" = z)
}

#pi =   function(t) {m <- 0.2 + 0.001 * t   #logit
#  z <- exp(m) / (1+ exp(m))
#  c("1" = z, "2" = 1 - z)}
#

`E[X|T,C]` = function(t, c)
{
  case_when(
    c == "1" ~ -3 + 0.1 * t,
    c == "2" ~ 10,
    TRUE ~ NaN
  )
}

t_dist = function(n){runif(n, min = 0, max = 16)}
attr(t_dist, "min") = 0
attr(t_dist, "max") = 16

sd_vector = c("1" = 1.5, "2" = 1) #0.5, 0.75, 1, 1.25

low_con = -1
high_con = 3 #errored out when this was 2^3
#RUN 1 : 2
#RUN 2: 3
#RUN 3: 4

scale = "log"

#formula = Surv(time = left_bound,
#               time2 = right_bound,
#               type = "interval2") ~ pspline(t, df = 0, calc = TRUE)
#formula2 = c == "1" ~ s(t)
#max_it = 3000
#ncomp = 2
#tol_ll = 1e-6
#maxiter_survreg = 30
#pi_function = TRUE
#pi_link = "identity"
#verbose = 2
#allow_safety = TRUE
#cutoff = 0.7
#fms_only = FALSE
#initial_weighting = 1
#keep_true_values = TRUE
#
#





df <- simulate_mics(
  #changed to test
  n = n,
  t_dist = t_dist,
  pi = pi,
  `E[X|T,C]` = `E[X|T,C]`,
  sd_vector = sd_vector,
  covariate_list = covariate_list,
  covariate_effect_vector = covariate_effect_vector,
  conc_limits_table = conc_limits_table,
  low_con = low_con,
  high_con = high_con,
  scale = scale
) %>% select(t, comp, observed_value, left_bound, right_bound, indicator) %>% mutate(
  cens =
    case_when(
      left_bound == -Inf ~ "lc",
      right_bound == Inf ~ "rc",
      TRUE ~ "int"
    ))

if(nrow(df %>% filter(left_bound == -Inf)) > 0){
  plot_min <- (df %>% filter(left_bound == -Inf) %>% pull(right_bound) %>% min) - 1
}else{
  plot_min <- (df %>% pull(left_bound) %>% min) - 1
}

if(nrow(df %>% filter(right_bound == Inf)) > 0){
  plot_max <- (df %>% filter(right_bound == Inf) %>% pull(left_bound) %>% max) + 1
}else{
  plot_max <- (df %>% pull(right_bound) %>% max) + 1
}

df %>%
  ggplot() +
  #geom_bar(aes(x = mid, fill = cens)) +
  geom_segment(aes(x = t, xend = t, y = left_bound, yend = right_bound, color = cens), data = (df %>% filter(left_bound != -Inf & right_bound != Inf))) +
  geom_segment(aes(x = t, xend = t, y = right_bound, yend = left_bound, color = cens), data = (df %>% filter(left_bound == -Inf & right_bound != Inf) %>% mutate(left_bound = right_bound - 1.5)), arrow = arrow(length = unit(0.03, "npc"))) +
  geom_segment(aes(x = t, xend = t, y = left_bound, yend = right_bound, color = cens), data = (df %>% filter(left_bound != -Inf & right_bound == Inf) %>% mutate(right_bound = left_bound + 1.5)), arrow = arrow(length = unit(0.03, "npc"))) +
  geom_point(aes(x = t, y = left_bound), data = df %>% filter(left_bound != -Inf)) +
  geom_point(aes(x = t, y = right_bound), data = df %>% filter(right_bound != Inf)) +
  ylim(plot_min - 0.5, plot_max + 0.5) +
  xlab("Time") +
  ylab("MIC")





