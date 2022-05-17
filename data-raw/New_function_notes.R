A <- tibble( y  = c(0, 1, 2, 3), x1 = c(0, 0, 1, 1))
B <- tibble(x2 = c(1,1,2,3))
lm(y~x1 + B$x2, data = A)

complist1 = list(
  f1 = function(t) {3 + t + 2*t^2 -sqrt(t)},
  f2 = function(t) {3*t}
)

pi = function(t) {z <- 0.5 + 0.2 * t
c(z, 1- z)}



mean_func1 = function(comp, complist = complist1, t)
{
  case_when(
    comp == 1 ~ complist[[1]](t),
    comp == 2 ~ complist[[2]](t))
}

gen_comp = function(t, p)
{
  map(p, ~sample.int(n = length(.x), size = 1, prob = .x, replace = TRUE))
}

t_dist1 = function(n)
{
  runif(n, min = 0, max = 1)
}

component_mean = function(
    n = 100,
    t_dist = t_dist1,
    pi = pi1,
    mean_func = mean_func1,
    comp_func = gen_comp)
{

  tibble(
    t = t_dist(n = n),
    p = map(t, ~pi(.x)),
    comp = gen_comp(t, p),
    x = mean_func(t = t, comp = comp)) %>%
    mutate(sd = sd[as.numeric(comp)]) %>%
    rowwise() %>%
    mutate(value = rnorm(1, x, sd)) %>%
    ungroup() %>%
    mutate(comp = as.double(as_vector(comp)))

}

sd = c(1, 2)

draw_observed_values <- function(n = 100,
                                 t_dist = t_dist1,
                                 pi = pi1,
                                 mean_func = mean_func1,
                                 comp_func = gen_comp,
                                 sd_vector = c(1,1)){
  component_mean(n, t_dist, pi, mean_func, comp_func) %>%
  mutate(sd = sd_vector[as.numeric(comp)]) %>%
  rowwise() %>%
  mutate(value = rnorm(1, x, sd)) %>%
  ungroup() %>%
  mutate(comp = as.double(as_vector(comp)))

}

  draw_observed_values()

  %>%
  mutate(z = (value - x) / sd) %>% select( !p) %>%
  ggplot2::ggplot() +
  ggplot2::geom_histogram(ggplot2::aes(x = z))

simulate_mics()


as_vector(a$comp)

a %>%
  select(comp) %>%
  filter(comp = 1)


p =
mapply(function(x) sample.int(n = length(p), size = length(t), prob = x,), p)

