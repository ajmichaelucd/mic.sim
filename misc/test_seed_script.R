Sys.setlocale (locale = "en_US.UTF-8")
print(sort(c("10", "1:")))

set.seed(3)
covariate_effect_vector <- c(0) #0 at start is intercept, then add in the desired coefficients for the covariates
covariate_list <-  NULL
covariate_names <- NULL
n=2000
ncomp = 2
pi1 = function(t) {z <- 0.5
c("1" = z, "2" = 1- z)}

`E[X|T,C]` = function(t, c)
{
  case_when(
    c == "1" ~ -1 + 0.1 * t,
    c == "2" ~ 1 + 0 * t,
    TRUE ~ NaN
  )
}

t_dist1 = function(n){runif(n, min = 0, max = 5)}

sd_vector = c("1" = 1, "2" = 0.5)

low_con = 2^-3
high_con = 2^2 #errored out when this was 2^3

scale = "log"

formula = Surv(time = left_bound,
               time2 = right_bound,
               type = "interval2") ~ 0 + c + strata(c) + t:c
max_it = 3000
ncomp = 2
tol_ll = 1e-6

arf_arf<- simulate_mics(n = n, t_dist = t_dist1, pi = pi1, `E[X|T,C]` = `E[X|T,C]`, sd_vector = sd_vector, covariate_list = covariate_list, covariate_effect_vector = covariate_effect_vector, low_con = low_con, high_con = high_con, scale = "log")

prep_sim_data_for_em(arf_arf , observed_value_choice = TRUE
                     ) %>%
  fit_model()







