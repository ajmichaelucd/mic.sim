

  model1 <- gam::gam(comp == "2" ~ s(t), family = binomial(link = "logit"), data = possible_data, weights = `P(C=c|y,t)`)
  gam::plot.Gam(model1, se = T, col = "green")


  zz <- seq(0, max(visible_data$t), len = 300)
  preds <- predict(model1, newdata = data.frame(t = zz), type = "response")
  tibble(t = zz, spline.pred.pi = preds) %>%
  ggplot(aes(t, spline.pred.pi)) + geom_point()

#  zz <- seq(0, max(visible_data$t), len = 300)
#  predict(binom_model, data.frame(t = zz))

  pred_sp <- function(value, model = model1){
    predict(model, newdata = data.frame(t = value), type = "response")
  }

  g <- tibble(t = zz, spline.pred.pi = preds)

plot1  <- ggplot() +
    geom_function(fun = pred_sp) +
    xlim(0, 1) +
    geom_point(aes(t, spline.pred.pi), data = g)


plot1 + expand_limits(y = c(0, 1))

  model2 <- mgcv::gam(c == "2" ~ s(t, bs = "ps"), family = binomial(link = "logit"), data = possible_data, weights = `P(C=c|y,t)`)
tidymv::predict_gam(model2) %>%
  ggplot(aes(t, fit)) +
  tidymv::geom_smooth_ci()











formula = Surv(time = left_bound,
               time2 = right_bound,
               type = "interval2") ~ 0 + c + strata(c) + pspline(t, df = 0, calc = TRUE):c

model <- survival::survreg(
  formula,  ##Make this chunk into an argument of the function
  weights = `P(C=c|y,t)`,
  data = df_temp,
  dist = "gaussian",
  control = survreg.control(maxiter = maxiter_survreg, debug = verbose > 3))

df_temp <- df_temp %>% mutate(c1t = case_when(
  c == "1" ~ t,
  TRUE ~ 2.5),
  c2t = case_when(
    c == "2" ~ t,
    TRUE ~ 2.5
  ),
  surv = Surv(time = left_bound,
              time2 = right_bound,
              type = "interval2")
)

formula = Surv(time = left_bound,
               time2 = right_bound,
               type = "interval2") ~ 0 + c + strata(c) + pspline(c1t, df = 0, calc = TRUE) + pspline(c2t, df = 0, calc = TRUE)

model_split <- survival::survreg(
  formula,  ##Make this chunk into an argument of the function
  weights = `P(C=c|y,t)`,
  data = df_temp,
  dist = "gaussian",
  control = survreg.control(maxiter = maxiter_survreg, debug = verbose > 3))
model_split

df = tibble(t = rep(seq(0, max(visible_data$t), len = 300), 2), c = factor(rep(1:2, each = 300))) %>% mutate(c1t = case_when(
  c == "1" ~ t,
  TRUE ~ 0.5),
  c2t = case_when(
    c == "2" ~ t,
    TRUE ~ 0.5
  )
)

tibble(pred = predict(model_split, df), t = df$t, c = df$c) %>% ggplot() +
  geom_point(aes(x = t, y = pred, color = c))







formula = Surv(time = left_bound,
               time2 = right_bound,
               type = "interval2") ~ pspline(t, df = 0, calc = TRUE)

df_temp %>% filter(c == 1) -> df1
df_temp %>% filter(c == 2) -> df2
modelsplit_1 <- survival::survreg(
  formula,  ##Make this chunk into an argument of the function
  weights = `P(C=c|y,t)`,
  data = df1,
  dist = "gaussian",
  control = survreg.control(maxiter = maxiter_survreg, debug = verbose > 3))
modelsplit_2 <- survival::survreg(
  formula,  ##Make this chunk into an argument of the function
  weights = `P(C=c|y,t)`,
  data = df2,
  dist = "gaussian",
  control = survreg.control(maxiter = maxiter_survreg, debug = verbose > 3))




df_pred_1 = tibble(t = seq(0, max(visible_data$t), len = 300), c = factor(rep(1, each = 300)))
df_pred_2 = tibble(t = seq(0, max(visible_data$t), len = 300), c = factor(rep(2, each = 300)))

rbind(tibble(pred = predict(modelsplit_1, df_pred_1), t = df_pred_1$t, c = df_pred_1$c),
tibble(pred = predict(modelsplit_2, df_pred_1), t = df_pred_2$t, c = df_pred_2$c)) %>% ggplot() +
  geom_point(aes(x = t, y = pred, color = c))






















######new plan, fit oracle logit, use those as weights, try to fit the spline models to that, including regression splines since those can be used in an interaction term in survreg!

pi = function(t) {m <- 0.6 + 0.06 * t ^ 2   #logit
#pi = function(t) {m <- 0.6 + 0.03 * t   #logit
z <- exp(m) / (1+ exp(m))
c("1" = 1 - z, "2" = z)}

n = 150
t_dist = function(n){runif(n, min = 0, max = 10)}
#pi = function(t) {z <- 0.6 #0.5 + 0.2 * t
#pi = function(t) {z <- 0.6 + 0.03 * t  ##identity
#c("1" = z, "2" = 1 - z)},
`E[X|T,C]` = function(t, c)
{
  case_when(
    c == "1" ~ -2 -0.1*t, #3 + t + 2*t^2 - sqrt(t),
    c == "2" ~ 2 + 0.2*t,
    TRUE ~ NaN
  )
}
sd_vector = c("1" = 1, "2" = 1)
covariate_list = NULL
covariate_effect_vector = c(0)
covariate_names = NULL
conc_limits_table = NULL #conc_limits_table = as_tibble(rbind(c("a", 2^-3, 2^3),
#c("b", 2^-4, 2^4)), `.name_repair` = "unique"
#) %>% rename("covariate_2" = 1, "low_cons" = 2, "high_cons" = 3),
low_con = -4
high_con = 4
scale = "log"
maxiter_survreg = 30

set.seed(129)

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




visible_data <- data.sim %>% mutate(obs_id = 1:n()) %>%
  relocate(obs_id, .before = everything())

median_y = median(visible_data$left_bound)

possible_data <-
  visible_data %>% #visible data with c for component
  reframe(.by = everything(),    #implement for other intial weighting options too ##########
          c = as.character(1:2)) %>%
  mutate(

    `P(C=c|y,t)` = case_when(    left_bound == -Inf & c == "2" ~ 0.01,
                                 left_bound == -Inf & c == "1" ~ 0.99,
      left_bound > median_y & c == "2" ~ (((left_bound - median_y) / (high_con - median_y)) * 0.5) + 0.5,
                             left_bound > median_y & c == "1" ~ 1 - ((((left_bound - median_y) / (high_con - median_y)) * 0.5) + 0.5),
                             left_bound <= median_y & left_bound != -Inf & c == "2" ~ 1 - ((((median_y - left_bound) / (median_y - low_con + 1)) * 0.5) + 0.5),
                             left_bound <= median_y & left_bound != -Inf & c == "1" ~ (((median_y - left_bound) / (median_y - low_con + 1)) * 0.5) + 0.5
                             ),
    mid =
      case_when(
        left_bound == -Inf ~ right_bound - 0.5,
        right_bound == Inf ~ left_bound + 0.5,
        TRUE ~ (left_bound + right_bound) / 2
      ),
    rc = ifelse(right_bound == Inf, TRUE, FALSE)
  ) %>% ungroup()


model_s <- visible_data %>% gam::gam(comp == "2" ~ s(t), family = binomial(link = "logit"), data = .)
model_lo <- visible_data %>% gam::gam(comp == "2" ~ gam::lo(t), family = binomial(link = "logit"), data = .)
#model1 <- visible_data %>% loess(comp == "2" ~ t, family = binomial(link = "logit"), data = .)
gam::plot.Gam(model_s, se = T, col = "green")
gam::plot.Gam(model_lo, se = T, col = "green")

#pi = function(t) {m <- 0.6 + 0.03 * t   #logit
#z <- exp(m) / (1+ exp(m))
#c("1" = 1 - z, "2" = z)}


tibble(
  t = seq(0, 10, length.out = 300),
  pred_s = (data.frame(t = seq(0, 10, length.out = 300)) %>% predict(model_s, ., type = "response")),
  pred_lo = (data.frame(t = seq(0, 10, length.out = 300)) %>% predict(model_lo, ., type = "response"))
) %>%
  mutate(actual = exp(0.6 + 0.06 * t ^ 2) / (1 + exp(0.6 + 0.06 * t ^ 2))) %>%
  ggplot() +
  geom_point(aes(x = t, y = pred_s), col = "black") +
  geom_point(aes(x = t, y = pred_lo), col = "violet") +
  geom_point(aes(x = t, y = actual), color = "red") +
  geom_smooth(aes(x = t, y = (comp == "2") * 1) , data = visible_data, method = "loess") +
  #  geom_smooth(aes(x = t, y = (comp == "2") * 1) , data = visible_data, method = "gam", formula = y ~ s(x, bs = "cs")) +
  ggplot2::expand_limits(y = c(0, 1)) +
  ggplot2::geom_rug(sides = "b",
                    aes(x = t, y = (comp == "2") * 1),
                    data = visible_data %>% filter(comp == "1")) +
  ggplot2::geom_rug(sides = "t",
                    aes(x = t, y = (comp == "2") * 1),
                    data = visible_data %>% filter(comp == "2")) +
  ggplot2::geom_hline(yintercept = mean((visible_data$comp == "2") * 1),
                      color = "yellow",
                      alpha = 0.7)


model1 <- model_s

possible_data <- possible_data %>% mutate(
  model_weights = case_when(c == "1" ~ 1 - gam::predict.Gam(model1, data.frame(t), type = "response"),
                             c == "2" ~ gam::predict.Gam(model1, data.frame(t), type = "response"),
                             TRUE ~ 0)
)


possible_data %>% select(obs_id, t, observed_value, left_bound, right_bound, c, oracle_weights)


oracle_newdata <- possible_data %>%
  #     select(-any_of("P(C = c)")) %>%
  #      left_join(pi, by = "c") %>%
  mutate(
    `E[Y|t,c]` = case_when(
      c == "2" ~ 2 + 0.2*t, #3 + t + 2*t^2 - sqrt(t),
      c == "1" ~ -2 - 0.1*t,
      TRUE ~ NaN
    ),
    `sd[Y|t,c]` = case_when(
      c == "2" ~ 1, #3 + t + 2*t^2 - sqrt(t),
      c == "1" ~ 1,
      TRUE ~ NaN
    ),
    # `Var[Y|t,c]` = `sd[Y|t,c]`^2,

    `P(Y|t,c)` =  if_else(
      left_bound == right_bound,
      dnorm(x = left_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`),
      pnorm(right_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`) -
        pnorm(left_bound, mean = `E[Y|t,c]`, sd =  `sd[Y|t,c]`)
    )
  ) %>% #select(obs_id, t, observed_value, c, `P(C=c|y,t)`, oracle_weights, `E[Y|t,c]`, `sd[Y|t,c]`, `P(Y|t,c)`)
  group_by(obs_id) %>%
  #  mutate(
  #    `P(c,y|t)` = `P(Y|t,c)` * `P(C = c)`, #unsure here
  #    `P(Y=y|t)` = sum(`P(c,y|t)`),
  #    `P(C=c|y,t)` = `P(c,y|t)` / `P(Y=y|t)`
  #  )
  mutate(`P(C=c|t)` = case_when( ########UNSURE ABOUT THIS SECTION
    c == "2" ~ predict(model1, newdata = tibble(t = t), type = "response"), ########UNSURE ABOUT THIS SECTION
    c == "1" ~ 1 - predict(model1, newdata = tibble(t = t), type = "response") ########UNSURE ABOUT THIS SECTION
  )) %>%  ########UNSURE ABOUT THIS SECTION
  mutate(`P(c,y|t)` = `P(C=c|t)` * `P(Y|t,c)`, ########UNSURE ABOUT THIS SECTION
         `P(Y=y|t)` = sum(`P(c,y|t)`), ########UNSURE ABOUT THIS SECTION
         `P(C=c|y,t)` = `P(c,y|t)` / `P(Y=y|t)`) %>%  ########UNSURE ABOUT THIS SECTION
  ungroup()



verbose = 3


formula = Surv(time = left_bound,
               time2 = right_bound,
               type = "interval2") ~ 0 + c + strata(c) + c:ns(t, df = 7)

nobs<- length(unique(oracle_newdata$obs_id))
assignments <- tibble(obs_id = sample(c(1:nobs), size = nobs, replace = FALSE)) %>% mutate(group = rep(1:10, length.out = nobs))
oracle_newdata <- oracle_newdata %>% left_join(., assignments)

for(i in 1:10){
  test <- oracle_newdata %>% filter(group == i)
  train <- oracle_newdata %>% filter(group != i)

  formula = Surv(time = left_bound,
                 time2 = right_bound,
                 type = "interval2") ~ 0 + c + strata(c) + c:ns(t, df = 7)

  modelns <- survival::survreg(
    formula,  ##Make this chunk into an argument of the function
    weights = `P(C=c|y,t)`,
    data = train,
    dist = "gaussian",
    control = survreg.control(maxiter = maxiter_survreg, debug = verbose > 3))



}



modelns <- survival::survreg(
  formula,  ##Make this chunk into an argument of the function
  weights = `P(C=c|y,t)`,
  data = oracle_newdata,
  dist = "gaussian",
  control = survreg.control(maxiter = maxiter_survreg, debug = verbose > 3))

modelns

df1 = tibble(t = rep(seq(0, max(possible_data$t), len = 300), 2), c = factor(rep(1:2, each = 300)))
dfns <- tibble(pred.ns = predict(modelns, df1), df1)

ggplot() +
  geom_point(aes(x = t, y = pred.ns, color = c), data = df2ns)


  df_temp <- oracle_newdata %>% filter(`P(C=c|y,t)` != 0 )
#possible_data <- possible_data %>% filter(`P(C=c|y,t)` != 0 )

  formula_split = Surv(time = left_bound,
                 time2 = right_bound,
                 type = "interval2") ~ pspline(t, df = 0, calc = TRUE)


#print("about to survreg")
  pred.ns = predict(modelns, df1)
df_temp %>% filter(c == 1) -> df1
df_temp %>% filter(c == 2) -> df2
modelsplit_1 <- survival::survreg(
  formula_split,  ##Make this chunk into an argument of the function
  weights = `P(C=c|y,t)`,
  data = df1,
  dist = "gaussian",
  control = survreg.control(maxiter = maxiter_survreg, debug = verbose > 3))
modelsplit_2 <- survival::survreg(
  formula_split,  ##Make this chunk into an argument of the function
  weights = `P(C=c|y,t)`,
  data = df2,
  dist = "gaussian",
  control = survreg.control(maxiter = maxiter_survreg, debug = verbose > 3))
#


tibble(t = rep(seq(0, max(possible_data$t), len = 300), 2)) %>%
  mutate(
    c1pred = predict(modelsplit_1, tibble(t), se = T)$fit,
    c1pred_se = predict(modelsplit_1, tibble(t), se = T)$se.fit,
    c1pred_lb = c1pred - 1.96 * c1pred_se,
    c1pred_ub = c1pred + 1.96 * c1pred_se,
    c2pred = predict(modelsplit_2, tibble(t), se = T)$fit,
    c2pred_se = predict(modelsplit_2, tibble(t), se = T)$se.fit,
    c2pred_lb = c2pred - 1.96 * c2pred_se,
    c2pred_ub = c2pred + 1.96 * c2pred_se,
  ) %>%
  ggplot() +
  geom_point(aes(x = t, y = c1pred), color = "black") +
  ggplot2::geom_ribbon(aes(x = t, ymin = c1pred_lb, ymax = c1pred_ub), alpha = 0.3) +
  ggplot2::geom_ribbon(aes(x = t, ymin = c2pred_lb, ymax = c2pred_ub), alpha = 0.3) +
  geom_point(aes(x = t, y = c2pred), color = "black") +
  geom_point(aes(x = t, y = pred.ns, color = c), data = dfns) +
  geom_point(aes(x = t, y = -2 - 0.1 * t), color = "violet") +
  geom_point(aes(x = t, y = 2 + 0.2 * t), color = "violet")




#`E[X|T,C]` = function(t, c)
#{case_when(
#    c == "1" ~ -2 -0.1*t, #3 + t + 2*t^2 - sqrt(t),
#    c == "2" ~ 2 + 0.2*t,
#    TRUE ~ NaN)}

#possible_data %>% filter(`P(C=c|y,t)` > 0.01 & ) ??

possible_data %>% filter(c == "1") %>% arrange(t) %>% select(t, `P(C=c|y,t)`, observed_value, comp)
hist(possible_data$t, breaks = 100)




oracle_newdata %>% filter(c == "1") %>% arrange(t) %>% select(t, `P(C=c|y,t)`, observed_value, comp) %>% print(n = 40)



