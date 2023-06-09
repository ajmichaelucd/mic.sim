t = seq(0, 10, length.out = 300)

tibble(t = seq(0, 10, length.out = 300)) %>% mutate(
y1 = predict(newmodel[[1]], .),
y2 = predict(newmodel[[2]], .)) %>%
ggplot()+
  geom_point(aes(x = t, y = y1), color = "red") +
  geom_point(aes(x = t, y = -2 - 0.01 * (t ^ 2)), color = "violet") +
  geom_point(aes(x = t, y = y2), color = "blue") +
  geom_point(aes(x = t, y = 2 + 0.2*t), color = "lightblue")


tibble(t, ypi = gam::predict.Gam(binom_model, data.frame(t))) %>%
  ggplot() +
  geom_point(aes(x = t, y = ypi)) +
  geom_point(aes(x = t, y = 0.6 + 0.02 * (t ^ 2) - 0.0015 * (t ^ 3)), color = "blue")


tibble(t, ypi = gam::predict.Gam(binom_model, data.frame(t), type = "response")) %>%
  ggplot() +
  geom_point(aes(x = t, y = ypi)) +
  geom_point(aes(x = t, y = (exp(0.6 + 0.02 * (t ^ 2) - 0.0015 * (t ^ 3)) / (1 + exp(0.6 + 0.02 * (t ^ 2) - 0.0015 * (t ^ 3))) ) ), color = "blue")


lines(x = t, y = 0.6 + 0.02 * (t ^ 2) - 0.0015 * (t ^ 3))

pi_f = function(t) {m <- 0.6 + 0.02 * (t ^ 2) - 0.0015 * (t ^ 3)   #logit
z <- exp(m) / (1+ exp(m))
c("1" = 1 - z, "2" = z)},
#pi = function(t) {z <- 0.6 + 0.03 * t  ##identity
#c("1" = z, "2" = 1 - z)},
`E[X|T,C]` = function(t, c)
{
  case_when(
    c == "1" ~ -2 - 0.01 * (t ^ 2), #3 + t + 2*t^2 - sqrt(t),
    c == "2" ~ 2 + 0.2*t,
    TRUE ~ NaN
  )
},
sd_vector = c("1" = 1, "2" = 1)
