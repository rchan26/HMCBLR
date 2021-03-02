#' Function to split a dataframe into C sub-dataframes
#'
#' @param dataframe data
#' @param y_col_index index for responses y in data frame
#' @param x_col_index index / indicies for input variables X in data frame
#' @param C number of subsets
#' @param as_dataframe determines whether or not to return the split data as dataframes or as responses and inputs
#'
#' @return split data
#'
#' @export
split_data <- function(dataframe, y_col_index, X_col_index, C, as_dataframe = T) {
  samples_per_sub_post <- nrow(dataframe)/C
  if (samples_per_sub_post%%1==0) {
    split_dataframe <- lapply(1:C, function(i) {
      dataframe[(((i-1)*samples_per_sub_post)+1):(i*samples_per_sub_post),]
    })
    print(paste('split_data: data was split into', C, 'equal sets with', samples_per_sub_post, 'number of samples'))
  } else {
    samples_per_sub_post <- floor(nrow(dataframe)/C)
    split_dataframe <- lapply(1:(C-1), function(i) {
      dataframe[(((i-1)*samples_per_sub_post)+1):(i*samples_per_sub_post),]
    })
    split_dataframe[[C]] <- dataframe[(((C-1)*samples_per_sub_post)+1):nrow(dataframe),]
    print(paste('split_data: data was split into', (C-1), 'sets with', samples_per_sub_post, 'number of samples,',
                'and 1 set with', nrow(split_dataframe[[C]]), 'number of samples'))
  }
  if (as_dataframe) {
    return(split_dataframe)
  } else {
    data <- lapply(1:C, function(c) list('y' = NA, 'X' = NA))
    for (i in 1:C) {
      data[[i]]$y <- split_dataframe[[i]][,y_col_index]
      data[[i]]$X <- as.matrix(cbind(rep(1, nrow(as.matrix(x = split_dataframe[[i]][,X_col_index]))), split_dataframe[[i]][,X_col_index]))
      colnames(data[[i]]$X)[1] <- 'intercept'
    }
    return(data)
  }
}
