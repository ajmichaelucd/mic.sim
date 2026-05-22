# Check Survreg Iteration Maxout

Returns a logical value if the number of iterations for survreg to
converge is equal to the maximum number of iterations specified

## Usage

``` r
check_survreg_iteration_maxout(mu_models, ncomp, maxiter_survreg)
```

## Arguments

- mu_models:

  list of survreg objects fitted to each component by the EM algorithm

- ncomp:

  number of components being fitted by the EM algorithm

- maxiter_survreg:

  maximum number of iterations for survreg to fit the model
