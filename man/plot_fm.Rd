% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_fm.R
\name{plot_fm}
\alias{plot_fm}
\title{Plot Output of Model Fitting}
\usage{
plot_fm(
  output,
  title = "",
  add_log_reg = FALSE,
  s_breakpoint = NA,
  r_breakpoint = NA,
  use_prior_step = FALSE,
  range_zoom = FALSE,
  plot_range = NULL,
  start_date = 0
)
}
\arguments{
\item{output}{List, output of a fit_EM()}

\item{title}{String, title of plot}

\item{add_log_reg}{Logical, add a curve to the pi (component weight) plot showing proportion resistant and susceptible}

\item{s_breakpoint}{String, represents S breakpoint (e.g. <= 2) on MIC scale (not log2 scale)}

\item{r_breakpoint}{String, represents R breakpoint (e.g. >= 32) on MIC scale (not log2 scale)}

\item{use_prior_step}{Logical, if one mu model did not converge, can try plotting mu models from previous step by setting this to TRUE}

\item{range_zoom}{Logical, zoom y axis to range of tested concentrations}

\item{plot_range}{Vector of length 2, minimum and maximum values of y axis of plot}

\item{start_date}{Integer, value at which x axis shoudl start (year).}
}
\description{
Takes output of a fit_EM() run and plots it.
Logistic regression fitting is in progress (currently does not accept covariates and non-linear term must be "t")
}
\examples{
data = simulate_mics()
output = fit_EM(model = "pspline",
approach = "full",
pre_set_degrees = c(4,4),
visible_data = data,
non_linear_term = "t",
covariates = NULL,
pi_formula = c == "2" ~ s(t),
max_it = 300,
ncomp = 2,
tol_ll = 1e-6,
pi_link = "logit",
verbose = 1,
model_coefficient_tolerance = 0.00001,
initial_weighting = 3,
sd_initial = 0.2
)
plot_fm(output = output, title = "Example", add_log_reg = TRUE, s_breakpoint = "<=1", r_breakpoint = ">=4")


}
