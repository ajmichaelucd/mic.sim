# simulate_mics

function that wraps together all the functions that determine t's
distribution, pi and its trends, trends in the mean, component draws,
epsilon, covariates, and censors the data

## Usage

``` r
simulate_mics(
  n = 300,
  t_dist = function(n) {
     runif(n, min = 0, max = 16)
 },
  pi = function(t) {
z <- 0.17 + 0.025 * t - 0.00045 * t^2
     tibble(`1` = 1 - z,
    `2` = z)
 },
  `E[X|T,C]` = function(t, c) {
     case_when(c == "1" ~ -4 + (0.24 * t) - (0.0055 *
    t^2), c == "2" ~ 3 + 0.001 * t, TRUE ~ NaN)
 },
  sd_vector = c(`1` = 1, `2` = 1.05),
  covariate_list = NULL,
  covariate_effect_vector = c(0),
  conc_limits_table = NULL,
  low_con = -3,
  high_con = 6,
  scale = "log"
)
```

## Arguments

- n:

  Number of observations

- t_dist:

  A function of n for drawing values of t

- pi:

  A function of time that returns a vector of weights that sum to 1.

- sd_vector:

  A vector with length equal to the number of components, with the
  elements named "1", "2",...

- covariate_list:

  List of covariates, each one has its own format, see examples of
  numeric and categorical covariates

- covariate_effect_vector:

  Vector of covariate effects corresponding to the covariates listed
  above

- conc_limits_table:

  If concentration limits vary by some covariate use this table to
  specify limits for each value of the covariate. Is right-joined to
  data by the covariate values

- low_con:

  If concentration limits are constant for all observations, used to set
  the lowest tested concentration on the log2(MIC) scale

- high_con:

  If concentration limits are constant for all observations, used to set
  the highest tested concentration on the log2(MIC) scale

- scale:

  What scale ("log" or "MIC") the data returned by simulate_mics is.
  Default is "log" which corresponds to log2(MIC)

- \`E\[X\|T, C\]\`:

  A function of time and component that returns a value of mu (component
  mean) for any given time and component

## Examples
