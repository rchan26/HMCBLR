#' HMC sampler for Logistic Regression model
#'
#' Sample from (sub-)posterior using Stan
#'
#' @param full_data_count a matrix or dataframe of the unique data with their
#'                        corresponding counts
#' @param C number of sub-posterior
#' @param prior_means prior for means of predictors
#' @param prior_variances prior for variances of predictors
#' @param power the exponent of the posterior (default is 1)
#' @param iterations number of iterations per chain
#' @param warmup number of burn in iterations
#' @param chains number of chains
#' @param seed seed number for random number generation
#' @param output boolean value: defaults to T, determines whether or not to
#'               print output to console
#'
#' @return samples from the (sub-)posterior target for the logistic regression model
#'
#' @export
hmc_sample_BLR <- function(full_data_count,
                           C,
                           prior_means,
                           prior_variances,
                           power = 1,
                           iterations,
                           warmup,
                           chains,
                           seed = sample.int(.Machine$integer.max, 1),
                           output = F) {
  if (!is.matrix(full_data_count) & !is.data.frame(full_data_count)) {
    stop("hmc_sample_BLR: full_data_count must be a matrix or data frame")
  } else if (!is.vector(prior_means)) {
    stop("hmc_sample_BLR: prior_means must be a vector")
  } else if (!is.vector(prior_variances)) {
    stop("hmc_sample_BLR: prior_variances must be a vector")
  }
  y <- full_data_count$y
  X <- as.matrix(subset(full_data_count, select = -c(y, count)))
  count <- full_data_count$count
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
  training_data <- list(nsamples = length(y),
                        p = (ncol(X)-1),
                        y = y,
                        X = X,
                        count = count,
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
                             control = list(adapt_delta = 0.99,
                                            max_treedepth = 20))
  } else {
    model <- rstan::sampling(object = stanmodels$bayes_logistic,
                             data = training_data,
                             iter = iterations,
                             warmup = warmup,
                             chains = chains,
                             verbose = FALSE,
                             refresh = 0,
                             seed = seed,
                             control = list(adapt_delta = 0.99,
                                            max_treedepth = 20))
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
#' @param data list of length C where each item is a list where for c=1,...,C,
#'             data_split[[c]]$full_data_count is a matrix or data frame of
#'             the unique data with their corresponding counts
#' @param C number of sub-posteriors
#' @param prior_means prior for means of predictors
#' @param prior_variances prior for variances of predictors
#' @param power the exponent of the posterior (default is 1)
#' @param seed seed number for random number generation
#' @param n_cores number of cores to use
#' @param output boolean value: defaults to T, determines whether or not
#'               to print output to console
#'
#' @return samples from the sub-posterior targets for the split data sets
#'         for the logistic regression model
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
                                 n_cores = parallel::detectCores(),
                                 output = F) {
  if (!is.list(data_split)) {
    stop("hmc_base_sampler_BLR: data_split must be a list")
  } else if (!all(sapply(1:length(data_split), function(i) is.data.frame(data_split[[i]]$full_data_count)))) {
    stop("hmc_base_sampler_BLR: for each i in 1:length(data_split), data_split[[i]]$full_data_count must be a data frame")
  } else if (!is.vector(prior_means)) {
    stop("hmc_base_sampler_BLR: prior_means must be a vector")
  } else if (!is.vector(prior_variances)) {
    stop("hmc_base_sampler_BLR: prior_variances must be a vector")
  }
  cl <- parallel::makeCluster(n_cores,
                              outfile = 'output_hmc_sample_BLR.txt')
  parallel::clusterExport(cl, envir = environment(), varlist = c(ls(), "hmc_sample_BLR", "seed"))
  base_samples <- parallel::parLapply(cl, X = 1:length(data_split), fun = function(c) {
    hmc_sample_BLR(full_data_count = data_split[[c]]$full_data_count,
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
