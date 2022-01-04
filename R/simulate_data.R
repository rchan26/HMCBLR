#' Logistic regression data simulator
#'
#' Simulate logistic regression data
#'
#' @param N number of samples
#' @param alpha intercept coefficient
#' @param frequencies frequency of coefficients occurring
#' @param coefficients coefficient terms
#' @param G_means mean of the Gaussian for simulating the data
#' @param G_sds standard deviations of the Gaussian for simulating the data
#' @param seed seed number for random number generation
#'
#' @return simulated data for logistic regression
#'
#' @export
simulate_LR_data <- function(N,
                             alpha,
                             frequencies,
                             coefficients,
                             G_means = rep(1, length(frequencies)),
                             G_sds = rep(1, length(frequencies)),
                             seed = NULL) {
  if (length(frequencies) != length(coefficients)) {
    stop("length of frequencies and length of coefficients are not the same")
  }
  if (!is.null(seed)) {
    set.seed(seed)
  }
  dim <- length(frequencies)
  X <- matrix(nrow = N, ncol = dim)
  for (d in 1:dim) {
    for (i in 1:N) {
      if (runif(1, 0, 1) < frequencies[d]) {
        X[i,d] <- rnorm(1, G_means[d], G_sds[d])
      } else {
        X[i,d] <- 0
      }
    }
  }
  y <- rep(0, N)
  for (i in 1:N) {
    beta_X <- alpha + sum(X[i,] * coefficients)
    prob <- 1 / (1+exp(-beta_X))
    y[i] <- rbinom(1, 1, prob = prob)
  }
  return(cbind(y, data.frame(X)))
}

#' Variable activity checker
#'
#' Checks the proportion or number of active variables for each column in the dataset
#' (i.e. non-zero entries)
#'
#' @param data matrix
#'
#' @export
check_activity <- function(data, proportion = T) {
  if (!is.data.frame(data)) {
    stop("check_activity: data is not in a data frame format")
  }
  # create new data frame with same column names
  active_df <- data.frame(matrix(nrow = 1, ncol = ncol(data)))
  colnames(active_df) <- colnames(data)
  # loop through columns and check how many are 'active'
  # i.e. have a 1 in the instance
  for (j in 1:ncol(data)) {
    if (proportion) {
      active_df[,j] <- sum(data[,j]!=0) / nrow(data)
    } else {
      active_df[,j] <- sum(data[,j]!=0)
    }
  }
  if (proportion) {
    print('proportion that each variable is active in the dataset:')  
  } else {
    print('number of active variables:')
  }
  for (j in 1:ncol(active_df)) {
    print(paste(colnames(active_df)[j], ':', active_df[1,j]))
  }
}
