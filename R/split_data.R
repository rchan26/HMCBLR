#' Function to obtain unique rows in the data
#'
#' @param y vector of responses
#' @param X design matrix
#'
#' @return list containing the unique rows of the full data and the unique rows
#'         of the design matrix with their corresponding counts
#'
#' @export
unique_row_count <- function(y, X) {
  full_data <- as.data.frame(cbind('y' = y, X))
  full_data_count <- as.data.frame(full_data %>% group_by_all %>% count)
  colnames(full_data_count) <- c(colnames(full_data_count)[1:(length(colnames(full_data_count))-1)], "count")
  design_count <- as.data.frame(as.data.frame(X) %>% group_by_all %>% count)
  colnames(design_count) <- c(colnames(design_count)[1:(length(colnames(design_count))-1)], "count")
  return(list('full_data_count' = full_data_count,
              'design_count' = design_count))
}

#' Function to split a dataframe into C sub-dataframes
#'
#' @param dataframe data
#' @param y_col_index index for responses y in data frame
#' @param x_col_index index / indices for input variables X in data frame
#' @param C number of subsets
#' @param as_dataframe determines whether or not to return the split data as dataframes
#'                     or as responses and inputs
#'
#' @return split data
#'
#' @export
split_data <- function(dataframe, y_col_index, X_col_index, C, as_dataframe = TRUE) {
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
    data <- lapply(1:C, function(c) list('y' = NA, 'X' = NA, 'full_data_count' = NA, 'design_count' = NA))
    for (i in 1:C) {
      data[[i]]$y <- split_dataframe[[i]][,y_col_index]
      data[[i]]$X <- as.matrix(cbind(rep(1, nrow(as.matrix(x = split_dataframe[[i]][,X_col_index]))), split_dataframe[[i]][,X_col_index]))
      counts <- unique_row_count(y = data[[i]]$y, X = data[[i]]$X)
      data[[i]]$full_data_count <- counts$full_data_count
      data[[i]]$design_count <- counts$design_count
    }
    return(data)
  }
}
