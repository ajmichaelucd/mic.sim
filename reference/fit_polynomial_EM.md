# Title

Title

## Usage

``` r
fit_polynomial_EM(
  pre_set_degrees = NULL,
  max_degree,
  degree_sets = "matched",
  visible_data,
  nfolds,
  non_linear_term = "t",
  covariates = NULL,
  pi_formula = c == "2" ~ s(t),
  max_it = 3000,
  ncomp = 2,
  tol_ll = 1e-06,
  pi_link = "logit",
  verbose = 3,
  model_coefficient_tolerance = 1e-05,
  initial_weighting = 8,
  sd_initial = 0.2
)
```

## Arguments

- sd_initial:
