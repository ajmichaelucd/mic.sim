set.seed(113)
n = 1000
t = runif(n, 0, 10)
epsilon = rnorm(n, 0,2)

relationship = function(t){
  5.3 + 0.02 * t^2
}

low_con = 5
high_con = 9

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
           type = "interval2") ~ bs(t, df = 3), dist = "gaussian", data = .)


data %>% ggplot() +
  geom_segment(aes(x = t, xend = t, y= left_bound, yend = right_bound), color = "lightblue") +
  geom_point(aes(x = t, y = y)) +
  geom_function(fun = function(t){predict(a, newdata = data.frame(t = t))}, aes(color = "Survreg")) +
  geom_function(fun = function(t){relationship(t)}, aes(color = "Truth"))




#detach("package:survival", unload = TRUE)
