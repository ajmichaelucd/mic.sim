set.seed(113)
n = 1000
t = runif(n, 0, 10)
epsilon = rnorm(n, 0,2)

relationship = function(t){
  5.3 + 0.2 * t
}

low_con = 5
high_con = 8

library(tibble)
library(dplyr)
library(ggplot2)
library(magrittr)


data = tibble(t, epsilon, y =relationship(t) + epsilon) %>%
  mutate(left_bound =
           case_when(
             floor(y) < low_con ~ -Inf,
             floor(y) > high_con ~ high_con,
             TRUE ~ floor(y)
           ),
         right_bound =
           case_when(
             ceiling(y) > high_con ~ Inf,
             ceiling(y) < low_con ~ low_con,
             TRUE ~ ceiling(y)
           ),
         left_bound_mgcv =
           case_when(
             floor(y) < low_con ~ low_con,
             #floor(y) >= low_con & floor(y) <= high_con ~ floor(y),
             floor(y) > high_con ~ high_con,
             TRUE ~ floor(y)
           ),
         right_bound_mgcv =
           case_when(
             floor(y) < low_con ~ -Inf,
             #floor(y) >= low_con & floor(y) <= high_con ~ floor(y),
             ceiling(y) > high_con ~ Inf,
             TRUE ~ ceiling(y)
           )
  )

data %>% ggplot() +
  geom_segment(aes(x = t, xend = t, y= left_bound, yend = right_bound), color = "lightblue") +
  geom_point(aes(x = t, y = y))

library(survival)
a = data %>%
  survreg(
    formula =
  Surv(time = left_bound,
       time2 = right_bound,
       type = "interval2") ~ t, dist = "gaussian", data = .)

detach("package:survival", unload = TRUE)

library(mgcv)
data$yi = cbind(data$left_bound_mgcv, data$right_bound_mgcv)
  b = mgcv::gam(yi~t,family=mgcv::cnorm(theta = -3,link = "identity"),data=data, scale = -1, method = "REML")

  ##won't let me do interval censoring with a lower bound of -Inf for some reason?

  b$reml.scale
  b$scale
  b$scale.estimated
  b$sig2
  b$control
  b$family$getTheta(TRUE)
detach("package:mgcv", unload = TRUE)

data %>%
  ggplot() +
  geom_segment(aes(x = t, xend = t, y= left_bound, yend = right_bound), color = "orange", alpha = 0.1) +
  geom_point(aes(x = t, y = y)) +
  geom_function(fun = function(t){predict(a, newdata = data.frame(t = t))}, aes(color = "Survreg")) +
  geom_function(fun = function(t){predict(b, newdata = data.frame(t = t))}, aes(color = "MGCV"))




pi = function(t){
  lp = 0.3 + 0.08 * t - 0.04 * (t^2) + 0.0029 * (t^3)
  p = exp(lp) / (1 + exp(lp))
  return(p)
}

ggplot() +
  geom_function(fun = pi, xlim = c(0, 10))

bin_data = tibble(t) %>%
  mutate(.by = t,
         y = rbinom(1, 1, pi(t)))

library(gam)
gam_model = gam(y ~ s(t), family = binomial(link = "logit"), data = bin_data)
bin_data %>%
  ggplot() +
  geom_function(fun = pi, xlim = c(0, 10), aes(color = "Truth")) +
  xlim(0,10)+
  ylim(0.2, 0.8)+
  geom_function(fun = function(t){predict(gam_model, newdata = data.frame(t = t), type = "response")}, aes(color = "Gam"))

detach("package:gam", unload = TRUE)

library(mgcv)
mgcv_model = gam(y ~ s(t), family = binomial, data = bin_data)
bin_data %>%
  ggplot() +
  geom_function(fun = pi, xlim = c(0, 10), aes(color = "Truth")) +
  xlim(0,10) +
  ylim(0.2, 0.8) +
  geom_function(fun = function(t){predict(mgcv_model, newdata = data.frame(t = t), type = "response")}, aes(color = "MGCV"))
detach("package:mgcv", unload = TRUE)

m <- ggplot() +
  geom_function(fun = pi, xlim = c(0, 10))

 m = m + geom_function(fun = function(t){predict(gam_model, newdata = data.frame(t = t), type = "response")}, aes(color = "Gam")) +

#dat <- gamSim(1,n=n)
#y <- round(dat$y)
#y <- cbind(y-.5,y+.5) ## set up to indicate interval censoring
#y[300,1] <- -Inf
#dat$yi <- y
#b <- gam(yi~s(x0)+s(x1)+s(x2)+s(x3),family=cnorm,data=dat)


   ciTools::add_pi()


#
