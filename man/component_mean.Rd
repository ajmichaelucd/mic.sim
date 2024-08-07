% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/component_mean.R
\name{component_mean}
\alias{component_mean}
\title{component_mean}
\usage{
component_mean(
  n = 100,
  t_dist = function(n) {
     runif(n, min = 0, max = 1)
 },
  pi = function(t) {
     z <- 0.5 + 0.2 * t
     tibble(`1` = z, `2` = 1 - z)
 },
  `E[X|T,C]` = function(t, c) {
     case_when(c == "1" ~ 3 + t + 2 * t^2 - sqrt(t), c ==
    "2" ~ 3 * t, TRUE ~ NaN)
 }
)
}
\arguments{
\item{`E[X|T, C]`}{}
}
\description{
component_mean
}
\keyword{internal}
