# Plot Data

Produces a basic plot of data generated from simulate_mics or
import_mics_with_metadata

## Usage

``` r
preview_data(
  data,
  title = "",
  y_min = NULL,
  y_max = NULL,
  ECOFF = NULL,
  ECOFF_scale = "MIC",
  covariate = NULL,
  covariate_title = "Legend"
)
```

## Arguments

- data:

  Tibble or data frame from simulate_mics or import_mics_with_metadata

- title:

  Title of plot

- y_min:

  Minimum value for plot, some extra space will be added below so the
  lowest tested concentration is a reasonable value

- y_max:

  Maximum value for plot, some extra space will be added below so the
  highest tested concentration is a reasonable value

- ECOFF_scale:

  defaults to "MIC", meaning the ECOFF provided will be in concentration
  directly, if you have already taken log2(ECOFF) then change this to
  "log"

- covariate:

  string, name of a column in data

- covariate_title:

  what to name the legend for the covariate
