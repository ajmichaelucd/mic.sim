n = 150
t_dist = function(n){runif(n, min = 0, max = 5)}
pi = function(t) {m <- 0.6 + 0.03 * t   #logit
                  z <- exp(m) / (1+ exp(m))
                  c("1" = z, "2" = 1 - z)}
#pi = function(t) {z <- 0.6 + 0.03 * t  ##identity
#c("1" = z, "2" = 1 - z)}
###need to make model able to flip between logit and identity too!
pi_link = "logit"

`E[X|T,C]` = function(t, c)
{
  case_when(
    c == "1" ~ -2 - 0.1 * t, #3 + t + 2*t^2 - sqrt(t),
    c == "2" ~ 2 + 0.2 * t,
    TRUE ~ NaN
  )
}
sd_vector = c("1" = 1, "2" = 2)
covariate_list = NULL
covariate_effect_vector = c(0)
covariate_names = NULL
conc_limits_table = NULL #conc_limits_table = as_tibble(rbind(c("a", 2^-3, 2^3),
#c("b", 2^-4, 2^4)), `.name_repair` = "unique"
#) %>% rename("covariate_2" = 1, "low_cons" = 2, "high_cons" = 3),
low_con = -4
high_con = 4
scale = "log"
formula = Surv(time = left_bound,
               time2 = right_bound,
               type = "interval2") ~ 0 + c + strata(c) + t:c
max_it = 3000
ncomp = 2
tol_ll = 1e-6
#silent = FALSE,
maxiter_survreg = 30
verbose = 3
allow_safety = TRUE
cutoff = 0.9
fms_only = FALSE
initial_weighting = 1


set.seed(10)

data.sim <- simulate_mics( #changed to test
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
  scale = scale)


visible_data <- prep_sim_data_for_em(data.sim, left_bound_name = "left_bound", right_bound_name = "right_bound", time = "t", covariate_names, scale = scale)


fit_model_con_pi(
    visible_data,
    formula = Surv(time = left_bound,
                   time2 = right_bound,
                   type = "interval2") ~ 0 + c + strata(c) + t:c,
    formula2 = c == "2" ~ t,
    max_it = 3000,
    ncomp = 2,
    tol_ll = 1e-6,
    browse_at_end = FALSE,
    browse_each_step = FALSE,
    plot_visuals = FALSE,
    #silent = FALSE,
    verbose = 3,
    #low_con = 2^-3,
    #high_con = 2^3,
    maxiter_survreg = 30,
    initial_weighting = 1)




