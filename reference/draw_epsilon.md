# draw_epsilon

draw_epsilon

## Usage

``` r
draw_epsilon(
  n = 100,
  t_dist = function(n) {
     runif(n, min = 0, max = 1)
 },
  pi = function(t) {
z <- 0.5 + 0.2 * t
     tibble(`1` = z, `2` = 1 - z)
 },
  `E[X|T,C]` = function(t, c) {
     case_when(c == "1" ~ 3 + t + 2 * t^2 - sqrt(t), c ==
    "2" ~ 3 * t, TRUE ~ NaN)
 },
  sd_vector = c(`1` = 1, `2` = 2)
)
```

## Arguments

- sd_vector:
