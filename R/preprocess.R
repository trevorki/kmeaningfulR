# author: Sasha Babicki
# date: 2021-03-11
#
# Module to apply data preprocessing
#

#' This function takes in training data and applies some preprocessing steps
#' such as scaling and imputation
#'
#' @param X Array-like (Dataframe, Matrix, Array) Unprocessed numeric data
#'
#' @return An array representing the data after appropriate preprocessing steps are applied
#' @export
#'
#' @examples
#' X = data.frame(1, 2)
#' preprocess(X)
preprocess <- function(X){

  # Throw error for empty dataframe, alternative is to return empty
  if(length(X) < 1){
    stop("Please provide a dataframe X with at least one row as input")
  }

  # Throw error for non numeric data
  if(!any(sapply(X, is.numeric))){
    stop("All data must be numeric")
  }

  # Throw error if input is not array-like
  if(sum(dim(X)) < 2){
    stop("Input format does not have enough dimensions")
  }
  tryCatch(
    expr = {
      df <- as.data.frame(X)
    },
    error = function(e){
      stop("Input format not accepted")
    }
  )

  # Throw error if all values are NAs
  if(all(is.na(X))){
    stop("Please provide at least one non-null value in each column")
  }

  # impute numeric
  df[] <- lapply(df, function(x) replace(x, is.na(x), mean(x, na.rm = TRUE)))

  # only scale if more than one element
  if(sum(dim(df)) > 2){
    scaled_df <- scale(df, scale=TRUE, center=FALSE)
    scaled_df[is.na(scaled_df)] <- 0
    sd <- sd(scaled_df)
    if(sd == 0){
      sd <- 1
    }
    df <- (scaled_df-mean(scaled_df))/sd
  }

  # remove attributes and return as array
  as.matrix(as.data.frame(df))
  # array(as.data.frame(df))
}

