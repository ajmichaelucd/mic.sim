# Intermediate function to prepare simulated data for use in EM algorithm

Intermediate function to prepare simulated data for use in EM algorithm

## Usage

``` r
prep_sim_data_for_em(
  data.sim = simulate_mics(),
  left_bound_name = "left_bound",
  right_bound_name = "right_bound",
  time = "t",
  covariate_names = NULL,
  scale = NULL,
  keep_truth = FALSE,
  observed_value_name = "observed_value",
  comp_name = "comp",
  low_con_name = "low_con",
  high_con_name = "high_con"
)
```

## Arguments

- high_con_name:
