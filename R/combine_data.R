#' Function to combine data
#'
#' @param list_of_data the data that you wish to combine. Each element in this
#'                     list must be another list with responses y, design matrix X,
#'                     unique row counts of full data and unique row counts of
#'                     design matrix
#' @param dim dimension
#'
#' @return combined data
#'
#' @export
combine_data <- function(list_of_data, dim) {
  if (!is.list(list_of_data)) {
    stop("combine_data: list_of_data must be a list")
  } else if (!all(sapply(list_of_data, function(sub_posterior) (is.list(sub_posterior) & identical(names(sub_posterior), c("y", "X", "full_data_count", "design_count")))))) {
    stop("combine_data: each item in list_of_data must be a list of length 4 with names \'y\', \'X\', \'full_data_count\', \'design_count\'")
  } else if (!all(sapply(1:length(list_of_data), function(i) is.vector(list_of_data[[i]]$y)))) {
    stop("combine_data: for each i in 1:length(list_of_data), list_of_data[[i]]$y must be a vector")
  } else if (!all(sapply(1:length(list_of_data), function(i) is.matrix(list_of_data[[i]]$X)))) {
    stop("combine_data: for each i in 1:length(list_of_data), list_of_data[[i]]$X must be a matrix")
  } else if (!all(sapply(1:length(list_of_data), function(i) ncol(list_of_data[[i]]$X)==dim))) {
    stop("combine_data: for each i in 1:length(list_of_data), ncol(list_of_data[[i]]$X) must be equal to dim")
  } else if (!all(sapply(1:length(list_of_data), function(i) is.data.frame(list_of_data[[i]]$full_data_count)))) {
    stop("combine_data: for each i in 1:length(list_of_data), list_of_data[[i]]$full_data_count must be a data frame")
  } else if (!all(sapply(1:length(list_of_data), function(i) is.data.frame(list_of_data[[i]]$design_count)))) {
    stop("combine_data: for each i in 1:length(list_of_data), list_of_data[[i]]$design_count must be a data frame")
  }
  y <- unlist(lapply(list_of_data, function(sub_posterior) sub_posterior$y))
  X <- do.call(rbind, lapply(list_of_data, function(sub_posterior) sub_posterior$X))
  counts <- unique_row_count(y = y, X = X)
  return(c(list('y' = y, 'X' = X), counts))
}
