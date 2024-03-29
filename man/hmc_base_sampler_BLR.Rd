% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hmc_BLR_sampler.R
\name{hmc_base_sampler_BLR}
\alias{hmc_base_sampler_BLR}
\title{HMC sampler for base level for logistic regression model}
\usage{
hmc_base_sampler_BLR(
  nsamples,
  warmup,
  data_split,
  C,
  prior_means,
  prior_variances,
  power = 1,
  seed = sample.int(.Machine$integer.max, 1),
  n_cores = parallel::detectCores(),
  output = F
)
}
\arguments{
\item{nsamples}{number of samples per node}

\item{warmup}{number of burn in iterations}

\item{C}{number of sub-posteriors}

\item{prior_means}{prior for means of predictors}

\item{prior_variances}{prior for variances of predictors}

\item{power}{the exponent of the posterior (default is 1)}

\item{seed}{seed number for random number generation}

\item{n_cores}{number of cores to use}

\item{output}{boolean value: defaults to T, determines whether or not
to print output to console}

\item{data}{list of length C where each item is a list where for c=1,...,C,
data_split[\link{c}]$full_data_count is a matrix or data frame of
the unique data with their corresponding counts}
}
\value{
samples from the sub-posterior targets for the split data sets
for the logistic regression model
}
\description{
Sample for base level for logistic regression
}
