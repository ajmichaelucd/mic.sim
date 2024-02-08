#mgcv::multinom
library(mgcv)
set.seed(6)
## simulate some data from a three class model
n <- 1000
f1 <- function(x){sin(3*pi*x)*exp(-x)}
f2 <- function(x) x^3
f3 <- function(x) .5*exp(-x^2)-.2
f4 <- function(x) 1
x1 <- runif(n)
x2 <- runif(n)
eta1 <- 2*(f1(x1) + f2(x2)) - .5
eta2 <- 2*(f3(x1) + f4(x2)) - 1
p <- exp(cbind(0,eta1,eta2))
p <- p/rowSums(p) ## prob. of each category
cp <- t(apply(p,1,cumsum)) ## cumulative prob.
## simulate multinomial response with these probabilities
## see also ?rmultinom
y <- apply(cp,1,function(x) min(which(x>runif(1))))-1
## plot simulated data...
plot(x1,x2,col=y+3)

## now fit the model...
b <- gam(list(y~s(x1)+s(x2),~s(x1)+s(x2)),family=multinom(K=2))
plot(b,pages=1)
gam.check(b)


mu_formula = Surv(time = left_bound, time2 = right_bound, type = "interval2") ~
  pspline(t, df = 0, caic = T)

ncomp = 3

#pi_formula = c == "2" ~ s(t)

visible_data = simulate_mics(n=300,
              pi = function(t){
  z = 0.2 + 0.01 * t
  b = 0.2 + 0.001 * t
  tibble(`1` = z, `2` = b, `3` = 1 - z -b)
},
`E[X|T,C]` = function(t, c) {
  case_when(c == "1" ~ -3,
            c == "2" ~ 0,
            c == "3" ~ 3,
            TRUE ~ NaN)
},
sd_vector = c(`1` = 0.7, `2` = 0.7, `3` = 0.7),
covariate_list = NULL,
covariate_effect_vector = c(0),
low_con = -5,
high_con = 5,
scale = "log"
) %>% prep_sim_data_for_em(keep_truth = TRUE)

three_comp_iw = function(visible_data, ncomp, sd_parameter = 0.2){

  if(ncomp != 3){
    errorCondition("This initial weighting scheme is appropriate for 3 component models")
  }

  visible_data <- visible_data %>% mutate(cens = case_when(
    left_bound == -Inf | right_bound == low_con ~ "lc",
    right_bound == Inf |
      left_bound == high_con ~ "rc",
    TRUE ~ "int"
  ))

  n_obs <- nrow(visible_data)

  full_set = tibble(
    cens = c("rc", "lc", "int")
  )

#  cens_counts <-
#    visible_data %>%
#    summarize(.by = cens,
#              n = n()
#    ) %>% right_join(., full_set, by = join_by(cens)) %>% mutate(n = case_when(
#      is.na(n) ~ 0,
#      TRUE ~ n
#    )) %>% pivot_wider(
#      names_from = cens, values_from = n
#    )
#
#  lc <- cens_counts %>% pull(lc)
#  int <- cens_counts %>% pull(int)
#  rc <- cens_counts %>% pull(rc)
#  `P(1)` <- (lc + (0.5 * int) + 1) / (n_obs + 2)
#  `P(2)` <- (rc + (0.5 * int) + 1) / (n_obs + 2)

  `P(1)` = 1/3
  `P(2)` = 1/3
  `P(3)` = 1/3

  visible_data %>%
    reframe(.by = everything(),
            c = as.character(1:ncomp)
    ) %>%
    mutate(
      `E[Y|t,c]` = case_when(c == "1" ~ low_con,
                             c == "2" ~ ((high_con - low_con)/2) + low_con,
                             c == "3" ~ high_con,
                             TRUE ~ NaN),
      `sd[Y|t,c]` = case_when(c == "1" ~ sd_parameter * (2/3) * (high_con - low_con),
                              c == "2" ~  sd_parameter * (2/3) * (high_con - low_con),
                              c == "3" ~  sd_parameter * (2/3) * (high_con - low_con),
                              TRUE ~ NaN),
      `P(Y|t,c)` = case_when(
        left_bound == right_bound ~ dnorm(x = left_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`),
        left_bound <= `E[Y|t,c]` ~ pnorm(right_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`) -
          pnorm(left_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`),
        TRUE ~ pnorm(left_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`, lower.tail = FALSE) -
          pnorm(right_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`, lower.tail = FALSE)
      )
    ) %>%
    mutate(`P(C=c|t)` = case_when(
      c == "3" ~ `P(3)`,
      c == "2" ~ `P(2)`,
      c == "1" ~ `P(1)`
    ),
    `P(c,y|t)` = `P(C=c|t)` * `P(Y|t,c)`) %>%
    mutate(.by = obs_id,
           `P(Y=y|t)` = sum(`P(c,y|t)`)) %>%
    mutate(
      `P(C=c|y,t)` = `P(c,y|t)` / `P(Y=y|t)`) %>%
    return()
}

possible_data = three_comp_iw(visible_data, ncomp = 3, sd_parameter = 0.2)


likelihood_documentation <- matrix(data = NA, nrow = max_it, ncol = 3)
likelihood_documentation [,1] <- 1:max_it

for(i in 1:max_it){
  if(i != 1){
    old_mu_model <- mu_model
    old_pi_model <- pi_model
    old_log_likelihood <- log_likelihood
    old_possible_data <- possible_data
  }



 mu_model = map(1:ncomp, ~fit_mu_model(possible_data, pred_comp = .x, mu_formula))

 possible_data <- possible_data %>% mutate(cc = case_when(
   c == "2" ~ 2,
   c == "1" ~ 1,
   TRUE ~ 0
 ))


 rrr <- mgcv::gam(data = possible_data, formula =  list(cc ~ s(t), ~ s(t)), family = mgcv::multinom(K = 2), weights = `P(C=c|y,t)`)

predict(rrr, newdata = data.frame(t = 1), type="response")




}
