# author: Sasha Babicki
# date: 2021-03-05
#
# Module to apply data preprocessing
#

#' This function takes in training data and applies some preprocessing steps such as scaling
#'
#' @param X DataFrame Unprocessed data
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

  if(is.na(X)){
    stop("Please provide at least one non-null value in each column")
  }
  df

#  # auto-detect feature type
#  numeric_features = df.select_dtypes("number").columns
#  categorical_features = df.select_dtypes("object").columns
#
#  # impute and scale numeric features
#  numeric_transformer = make_pipeline(
#    SimpleImputer(),
#    StandardScaler()
#  )
#
#  # use OHE for all other features
#  categorical_transformer = make_pipeline(
#    SimpleImputer(missing_values=[None, np.nan],
#                  strategy="constant",
#                  fill_value=""),
#    OneHotEncoder(handle_unknown="ignore")
#  )
#
#  preprocessor = make_column_transformer(
#    (numeric_transformer, numeric_features),
#    (categorical_transformer, categorical_features)
#  )
#
#  X_processed = preprocessor.fit_transform(X)
#
#  return X_processed
}

