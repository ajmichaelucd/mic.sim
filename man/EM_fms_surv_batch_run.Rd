% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/EM_fms_surv_batch_run.R
\name{EM_fms_surv_batch_run}
\alias{EM_fms_surv_batch_run}
\title{Title}
\usage{
EM_fms_surv_batch_run(
  random_seeds_vector,
  visible_data,
  mu_formula = Surv(time = left_bound, time2 = right_bound, type = "interval2") ~
    pspline(t, df = 0, caic = TRUE),
  pi_formula = c == "2" ~ s(t),
  censored_side = "RC",
  extra_row = FALSE,
  max_it = 500,
  ncomp = 2,
  tol_ll = 1e-06,
  pi_link = "logit",
  verbose = 1,
  model_coefficient_tolerance = 1e-05,
  maxiter_survreg = 30,
  sd_initial = 0.2,
  randomize = "all"
)
}
\arguments{
\item{randomize}{}
}
\description{
Title
}
