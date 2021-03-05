# author: Mike Lynch
# date: 2021-03-05
#
# Module to optimize the choice of K in the K-means algorithm
#

#' This function takes in unlabeled, scaled data and performs clustering using the KMeans clustering algorithm values of K up to the min(10, n_samples - 1).
#' It returns the value for K which maximizes the mean silhouette scores across all clusters.
#'
#' @param X An array of unlabeled data with appropriate preprocessing steps applied.
#'
#' @return An integer for the optimal choice of K in the K-means algorithm
#' @export
#'
#' @examples
#' X <- data.frame(c(1, 3, 5), c(2, 4, 6))
#' X_scaled <- preprocess(X)
#' optimal_K <- find_elbow(X_scaled)
find_elbow <- function(X){
  
}
