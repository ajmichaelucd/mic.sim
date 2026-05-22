# Fit Mu Model

Function to filter the data to the desired component using the variable
c and fit survreg to the data using a formula supplied by the user,
typically passed in through the EM algorithm

## Usage

``` r
fit_mu_model(possible_data, pred_comp, mu_formula, maxiter_survreg = 30)
```

## Arguments

- possible_data:

  data frame iterated on by the EM algorithm, should contain left_bound,
  right_bound, c, P(C=c\|y,t), and any covariates, such as time, e.g.
  "t"

- pred_comp:

  numeric variable, which component is this model being fitted to, e.g.
  2

- mu_formula:

  formula being used to fit the model for mu, should contain a Surv
  object for an outcome and any covariates of interest, accepts pspline
  and other arguments to survreg

- maxiter_survreg:

  maximum number of iterations for survreg to fit the model
