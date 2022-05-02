##Matrix Form Approach

##Inputs
covariate_effect_vector <- c(0, #0 at start is intercept
                             0.01,
                             c(0.05, 0.08),
                             c(0.2, 0.3),
                             c(0.02, 0.04, 0.06, 0.08),
                             0.05
)

covariate_list <- list(
  c("numeric", "normal", 40, 4),
  c("categorical", 0.3, 0.4, 0.3),
  c("categorical", 0.5, 0.2, 0.3),
  c("categorical", 0.2, 0.2, 0.2, 0.2, 0.2),
  c("numeric", "uniform", 1, 10)
)


year = c(rep(0, 10), rep(1, 10), rep(2, 15))

##Operations

aaa <- find_epsilon(year = year, sd_1 = 1, sd_2 = 1, mean_1_trend = 0, mean_2_trend = 0.5, mean_1_intercept = -1, mean_2_intercept = 2, pi_1_trend = 0, pi_1_intercept = 0.4)

bbb <- add_covariate(covariate_list = covariate_list, year = year)

ccc <- tibble(aaa, bbb)

ddd <- covariate_effect_total(ccc, covariate_effect_vector)


tibble(ccc, ddd) %>%
  mutate(observed_value = epsilon + ddd)
