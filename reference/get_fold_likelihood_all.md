# Title

Title

## Usage

``` r
get_fold_likelihood_all(
  model = "surv",
  approach = "full",
  i,
  visible_data,
  degrees,
  non_linear_term = "t",
  covariates = NULL,
  pi_formula = c == "2" ~ s(t),
  fixed_side = NULL,
  extra_row = FALSE,
  ecoff = NA,
  max_it = 300,
  ncomp = 2,
  tol_ll = 1e-06,
  pi_link = "logit",
  verbose = 3,
  model_coefficient_tolerance = 1e-05,
  maxiter_survreg = 30,
  initial_weighting = 3,
  sd_initial = 0.2,
  scale = NULL,
  max_out_break = FALSE
)
```

## Arguments

- scale:
