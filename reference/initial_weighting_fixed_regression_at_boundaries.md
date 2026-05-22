# Intial Weighting Scheme: Fixed Regression at Boundaries

Fits regression lines with slope = 0 at high_con and low_con with sigma
values of 0.2 times the difference between high_con and low_con, and
calculates initial weights from this

## Usage

``` r
initial_weighting_fixed_regression_at_boundaries(
  visible_data,
  ncomp,
  sd_parameter = 0.2
)
```

## Arguments

- visible_data:

  the data frame passed into the EM algorithm, at minimum consists of:
  an id column, a time column, left_bound, right_bound, low_con, and
  high_con

- ncomp:

  number of components in the model being fitted, this weighting scheme
  only is valid for 2 component models
