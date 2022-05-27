#' HMC sampler for Logistic Regression model
#'
#' Sample from (sub-)posterior using Stan
#'
#' @param y vector of responses
#' @param X design matrix
#' @param C number of sub-posterior
#' @param prior_means prior for means of predictors
#' @param prior_variances prior for variances of predictors
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
hmc_sample_BLR_2 <- function(y,
                             X,
                             C,
                             prior_means,
                             prior_variances,
                             iterations,
                             warmup,
                             chains,
                             seed = sample.int(.Machine$integer.max, 1),
                             output = F) {
  if (!is.vector(prior_means)) {
    stop("hmc_sample_BLR_2: prior_means must be a vector")
  } else if (!is.vector(prior_variances)) {
    stop("hmc_sample_BLR_2: prior_variances must be a vector")
  }
  if (length(y) != nrow(X)) {
    stop("hmc_sample_BLR_2: y and X do not have the same number of samples")
  }
  # check that the design matrix does not have the intercept (i.e. first column identical to a vector of 1s)
  # reset rownames so that we can compare the first column to a vector of 1s
  rownames(X) <- c()
  if (identical(X[,1], rep(1, nrow(X)))) {
    X <- X[,2:ncol(X)]
    warning("hmc_sample_BLR_2: the first column of the design matrix included a column of 1s -
            the design matrix has been changed just include covariate columns")
  }
  dim <- ncol(X)+1
  if (length(prior_means)!=dim) {
    stop("hmc_sample_BLR_2: prior_means must be a vector of length ncol(X)+1
         (where X is design matrix without intercept)")
  } else if (length(prior_variances)!=dim) {
    stop("hmc_sample_BLR_2: prior_variances must be a vector of length ncol(X)+1
         (where X is design matrix without intercept")
  }
  print("Sampling from logistic regression model")
  training_data <- list(nsamples = length(y),
                        p = ncol(X),
                        y = y,
                        X = X,
                        prior_means = prior_means,
                        prior_variances = prior_variances,
                        C = C)
  if (output) {
    model <- rstan::sampling(object = stanmodels$bayes_logistic_2,
                             data = training_data,
                             iter = iterations,
                             warmup = warmup,
                             chains = chains,
                             seed = seed,
                             control = list(adapt_delta = 0.99,
                                            max_treedepth = 20))
  } else {
    model <- rstan::sampling(object = stanmodels$bayes_logistic_2,
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
#' @param data_split list of length C where each item is a list where for c=1,...,C,
#'                   data_split[[c]]$y is a vector of responses and data_split[[c]]$X
#'                   is the design matrix
#' @param C number of sub-posteriors
#' @param prior_means prior for means of predictors
#' @param prior_variances prior for variances of predictors
#' @param seed seed number for random number generation
#' @param n_cores number of cores to use
#' @param output boolean value: defaults to T, determines whether or not
#'               to print output to console
#'
#' @return samples from the sub-posterior targets for the split data sets
#'         for the logistic regression model
#'
#' @export
hmc_base_sampler_BLR_2 <- function(nsamples,
                                   warmup,
                                   data_split,
                                   C,
                                   prior_means,
                                   prior_variances,
                                   seed = sample.int(.Machine$integer.max, 1),
                                   n_cores = parallel::detectCores(),
                                   output = F) {
  if (!is.vector(prior_means)) {
    stop("hmc_base_sampler_BLR_2: prior_means must be a vector")
  } else if (!is.vector(prior_variances)) {
    stop("hmc_base_sampler_BLR_2: prior_variances must be a vector")
  }
  cl <- parallel::makeCluster(n_cores,
                              outfile = 'output_hmc_sample_BLR_2.txt')
  parallel::clusterExport(cl, envir = environment(), varlist = c(ls(), "hmc_sample_BLR_2", "seed"))
  base_samples <- parallel::parLapply(cl, X = 1:length(data_split), fun = function(c) {
    hmc_sample_BLR_2(y = data_split[[c]]$y,
                     X = data_split[[c]]$X,
                     C = C,
                     prior_means = prior_means,
                     prior_variances = prior_variances,
                     iterations = nsamples+warmup,
                     warmup = warmup,
                     chains = 1,
                     seed = seed,
                     output = output)})
  parallel::stopCluster(cl)
  return(base_samples)
}
