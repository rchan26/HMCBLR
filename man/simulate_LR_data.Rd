% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulate_data.R
\name{simulate_LR_data}
\alias{simulate_LR_data}
\title{Logistic regression data simulator}
\usage{
simulate_LR_data(
  N,
  alpha,
  frequencies,
  coefficients,
  G_means = rep(1, length(frequencies)),
  G_sds = rep(1, length(frequencies)),
  seed = NULL
)
}
\arguments{
\item{N}{number of samples}

\item{alpha}{intercept coefficient}

\item{frequencies}{frequency of coefficients occurring}

\item{coefficients}{coefficient terms}

\item{G_means}{mean of the Gaussian for simulating the data}

\item{G_sds}{standard deviations of the Gaussian for simulating the data}

\item{seed}{seed number for random number generation}
}
\value{
simulated data for logistic regression
}
\description{
Simulate logistic regression data
}
