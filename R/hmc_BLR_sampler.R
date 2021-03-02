#' HMC sampler for Logistic Regression model
#'
#' Sample from (sub-)posterior using Stan
#'
#' @param data list of length 2 where data$y is the vector for y responses
#'             and data$x is the design matrix for the covariates
#' @param C number of sub-posterior (default to 1)
#' @param prior_means prior for means of predictors
#' @param prior_variances prior for variances of predictors
#' @param power exponent for the posterior (defaults to 1)
#' @param iterations number of iterations per chain
#' @param warmup number of burn in iterations
#' @param chains number of chains
#' @param seed seed number for random number generation
#' @param output boolean value: defaults to T, determines whether or not to print output to console
#'
#' @return samples from the (sub-)posterior target for the logistic regression model
#'
#' @export
hmc_sample_BLR <- function(data,
                           C,
                           prior_means,
                           prior_variances,
                           power = 1,
                           iterations,
                           warmup,
                           chains,
                           seed = sample.int(.Machine$integer.max, 1),
                           output = F) {
  if (!is.list(data) | length(data)!=2) {
    stop("hmc_sample_BLR: data must be a length of length 2")
  } else if (!identical(names(data), c("y", "X"))) {
    stop("hmc_sample_BLR: data must be a length of length 2 with elements named \'y\' and \'X\'")
  } else if (!is.vector(data$y)) {
    stop("hmc_sample_BLR: data$y must be a vector")
  } else if (!is.matrix(data$X) & !is.data.frame(data$X)) {
    stop("hmc_sample_BLR: data$X must be a matrix or data frame")
  } else if (!is.vector(prior_means)) {
    stop("hmc_sample_BLR: prior_means must be a vector")
  } else if (!is.vector(prior_variances)) {
    stop("hmc_sample_BLR: prior_variances must be a vector")
  }
  y <- data$y
  X <- data$X
  # check that y and X have the same number of instances
  if (length(y) != nrow(X)) {
    stop("hmc_sample_BLR: y and X do not have the same number of samples")
  }
  dim <- ncol(X)
  if (length(prior_means)!=dim) {
    stop("hmc_sample_BLR: prior_means must be a vector of length ncol(X)")
  } else if (length(prior_variances)!=dim) {
    stop("hmc_sample_BLR: prior_variances must be a vector of length ncol(X)")
  }
  # check that the design matrix does have the intercept (i.e. first column not identical to a vector of 1s)
  # reset rownames so that we can compare the first column to a vector of 1s
  rownames(X) <- c()
  if (!identical(X[,1], rep(1, nrow(X)))) {
    X <- cbind(rep(1, nrow(X)), X)
    colnames(X)[1] <- 'intercept'
    warning("hmc_sample_BLR: the first column of the design matrix was not a column of 1s -
            the design matrix has been changed to include the intercept")
  }
  print("Sampling from logistic regression model")
  # function to use Stan (HMC) to sample from the tempered mixture Gaussian distribution
  training_data <- list(nsamples = length(y),
                        p = (ncol(X)-1),
                        y = y,
                        X = X,
                        prior_means = prior_means,
                        prior_variances = prior_variances,
                        C = C,
                        power = power)
  if (output) {
    model <- rstan::sampling(object = stanmodels$bayes_logistic,
                             data = training_data,
                             iter = iterations,
                             warmup = warmup,
                             chains = chains,
                             seed = seed,
                             control = list('adapt_delta' = 0.99))
  } else {
    model <- rstan::sampling(object = stanmodels$bayes_logistic,
                             data = training_data,
                             iter = iterations,
                             warmup = warmup,
                             chains = chains,
                             verbose = FALSE,
                             refresh = 0,
                             seed = seed,
                             control = list('adapt_delta' = 0.99))
  }
  print('Finished sampling from logistic regression model')
  return(rstan::extract(model)$beta)
}

#' HMC sampler for base level for logistic regression model
#'
#' Sample for base level for logistic regression
#'
#' @param nsamples number of samples per node
#' @param warmup number of burn in iterations
#' @param data list of length C where each item is a list of length 2 where
#'             for c=1,...,C, data[[c]]$y is the vector for y responses and
#'             data[[c]]$x is the design matrix for the covariates for sub-posterior c
#' @param C number of sub-posteriors (default to 1)
#' @param prior_means prior for means of predictors
#' @param prior_variances prior for variances of predictors
#' @param power exponent for the posterior (defaults to 1)
#' @param seed seed number for random number generation
#' @param output boolean value: defaults to T, determines whether or not to print output to console
#'
#' @return samples from the sub-posterior targets for the split data sets for the logistic regression model
#'
#' @export
hmc_base_sampler_BLR <- function(nsamples,
                                 warmup,
                                 data_split,
                                 C,
                                 prior_means,
                                 prior_variances,
                                 power = 1,
                                 seed = sample.int(.Machine$integer.max, 1),
                                 output = F) {
  if (!is.list(data_split) | length(data_split)!=C) {
    stop("hmc_base_sampler_BLR: data_split must be a list of length m")
  } else if (!all(sapply(data_split, function(sub_posterior) (is.list(sub_posterior) & identical(names(sub_posterior), c("y", "X")))))) {
    stop("hmc_base_sampler_BLR: each item in data_split must be a list of length 2 with names y and X")
  } else if (!all(sapply(1:C, function(i) is.vector(data_split[[i]]$y)))) {
    stop("hmc_base_sampler_BLR: for each i in 1:C, data_split[[i]]$y must be a vector")
  } else if (!all(sapply(1:C, function(i) is.matrix(data_split[[i]]$X)))) {
    stop("hmc_base_sampler_BLR: for each i in 1:C, data_split[[i]]$X must be a matrix")
  } else if (!is.vector(prior_means)) {
    stop("hmc_base_sampler_BLR: prior_means must be a vector")
  } else if (!is.vector(prior_variances)) {
    stop("hmc_base_sampler_BLR: prior_variances must be a vector")
  }
  cl <- parallel::makeCluster(C, setup_strategy = "sequential", outfile = 'output_hmc_sample_BLR.txt')
  parallel::clusterExport(cl, varlist = list("hmc_sample_BLR", "seed"))
  base_samples  <- parallel::parLapply(cl, X = 1:C, fun = function(i) {
    hmc_sample_BLR(data = data_split[[i]],
                   C = C,
                   prior_means = prior_means,
                   prior_variances = prior_variances,
                   power = power,
                   iterations = nsamples+warmup,
                   warmup = warmup,
                   chains = 1,
                   seed = seed,
                   output = output)})
  parallel::stopCluster(cl)
  return(base_samples)
}
