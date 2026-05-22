# Write Formulas for Mu Models

Used to write the formulas for mu models, returns either a single
formula or a list of formulas.

## Usage

``` r
write_all_formulas(non_linear_term, degrees, covariates, model)
```

## Arguments

- non_linear_term:

  String, non-linear term to be included in the model. Variable in the
  pspline term in the pspline model or in the polynomial term in the
  polynomial model.

- degrees:

  Vector of length equal to the number of component means being
  estimated. Elements are numeric and correspond to the number of
  degrees (polynomial) or degrees of freedom (pspline) in each mu model.
  First element corresponds to lowest component, last element
  corresponds to highest component.

- covariates:

  String, covariates to be included in mu model aside from the
  non-linear term.

- model:

  String, "pspline" or "polynomial". Which non-linear term should be
  used in model
