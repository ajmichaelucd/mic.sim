lll <- simulate_mics(
  n = 100,
  t_dist = function(n) {
    runif(n, min = 0, max = 3)
  },
  pi = function(t) {
    z <- 1.0
    c(`1` = z, `2` = 1 - z)
  },
  `E[X|T,C]` = function(t, c) {
    case_when(c == "1" ~ 0 + t ,
              c == "2" ~ 3 * t,
              TRUE ~ NaN)
  },
  sd_vector = c(`1` = 1, `2` = 2),
  covariate_list = NULL,
  covariate_effect_vector = c(0),
  low_con = 2^-4,
  high_con = 2^4,
  tested_concentrations = log2(low_con):log2(high_con),
  scale = "log"
)
mmm <- Surv(lll$left_bound, lll$right_bound, type = "interval2")

survival::survreg(mmm ~ t, data = lll, dist = "gaussian", debug = TRUE, iter.max = 4)
survreg.control()
