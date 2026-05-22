# mic.sim

An R package for modeling MIC data

## How to install

``` r

library(devtools)
install_github('ajmichaelucd/mic.sim', build_vignettes = FALSE)
```

Data can be simulated with
[`simulate_mics()`](https://ajmichaelucd.github.io/mic.sim/reference/simulate_mics.md)
or imported with
[`import_mics_with_metadata()`](https://ajmichaelucd.github.io/mic.sim/reference/import_mics_with_metadata.md).
Model fitting is done with
[`fit_EM()`](https://ajmichaelucd.github.io/mic.sim/reference/fit_EM.md).
Plotting of model fitting results uses
[`plot_fm()`](https://ajmichaelucd.github.io/mic.sim/reference/plot_fm.md)
and
[`plot_likelihood()`](https://ajmichaelucd.github.io/mic.sim/reference/plot_likelihood.md).

``` r

simulate_mics() |> 
  fit_EM(visible_data = _, preset_degrees = c(4,2)) |>
    plot_fm(output = _)
```
