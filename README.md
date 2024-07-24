# mic.sim
An R package for modeling MIC data

## How to install

```r
library(devtools)
install_github('ajmichaelucd/mic.sim', build_vignettes = FALSE)
```

Data can be simulated with `simulate_mics()` or imported with `import_mics_with_metadata()`. Model fitting is done with `fit_EM()`. Plotting of model fitting results uses `plot_fm()` and `plot_likelihood()`.

```r
simulate_mics() |> 
  fit_EM(visible_data = _, preset_degrees = c(4,2)) |>
    plot_fm(output = _)
```
