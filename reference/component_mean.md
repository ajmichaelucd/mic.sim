# component_mean

component_mean

## Usage

``` r
component_mean(
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
 }
)
```

## Arguments

- \`E\[X\|T, C\]\`:
