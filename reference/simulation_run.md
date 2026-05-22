# Title

Title

## Usage

``` r
simulation_run(
  i = 100,
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
  scale = "log",
  model = "pspline",
  approach = "full",
  pi_formula = c == "2" ~ s(t),
  ncomp = 2,
  ecoff = NA,
  pre_set_degrees = NULL,
  max_degree = 8,
  degree_sets = "matched",
  nfolds = 10,
  non_linear_term = "t",
  covariates = NULL,
  fixed_side = NULL,
  extra_row = FALSE,
  max_it = 3000,
  tol_ll = 1e-06,
  pi_link = "logit",
  verbose = 3,
  model_coefficient_tolerance = 1e-05,
  maxiter_survreg = 30,
  initial_weighting = 3,
  sd_initial = 0.2,
  reruns_allowed = 3,
  max_out_break = FALSE
)
```

## Arguments

- i:

  Seed, used when running batches of simulations

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

- max_out_break:
