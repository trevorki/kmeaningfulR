# author: Mike Lynch
# date: 2021-03-05
#
# Module for optimizing the choice of K in the K-means algorithm
#


#' avg_sil_score
#' This function takes in a vector of cluster labels and a corresponding array of points and calculates the average silhouette score across all clusters.
#' It returns the value of the average silhouette score.
#'
#' @param labels vector: A vector of cluster labels.
#' @param X matrix: A (n_points x n_features) matrix of points.
#'
#' @return numeric: The average silhouette score.
#' @export
#'
#' @examples
#' helper_data <- array(c(c(0, 10, 10), c(0, 10, 11)), dim = c(3,2))
#' helper_clusters <- c(1, 2, 2)
#' avg_sil_score(helper_clusters, helper_data)
avg_sil_score <- function(labels, X) {
  # verify input labels is of correct format
  if (!(is.vector(labels))){
    stop("Input `labels` should be a vector of numbers")
  }
  if (!(is.numeric(labels))){
    stop("Input `labels` should be a vector of numbers")
  }
  # verify input X is of correct format
  if (!(is.matrix(X))){
    stop("Input `X` should be a matrix of numbers")
  }
  if (!(is.numeric(X))){
    stop("Input `X` should be a matrix of numbers")
  }
  sil_scores <- cluster::silhouette(labels, stats::dist(X))
  mean(sil_scores[, 3])
}

#' find_elbow
#' This function takes in unlabeled, scaled data and performs clustering using the KMeans clustering algorithm values of K up to the min(10, n_samples - 1).
#' It returns the value for K which maximizes the mean silhouette scores across all clusters.
#'
#' @param X matrix: unlabeled data with appropriate preprocessing steps applied.
#'
#' @return numeric: The optimal choice of K in the K-means algorithm according to silhouette score.
#' @export
#'
#' @examples
#' X <- array(c(c(0, 1, 10, 10, 10, 10), c(0, 1, 10, 11, 0, 1)), dim = c(6,2))
#' X_scaled <- preprocess(X)
#' optimal_K <- find_elbow(X_scaled)
find_elbow <- function(X) {
  # verify input X is of correct format
  if (!(is.matrix(X))){
    stop("Input `X` should be a matrix of numbers")
  }
  if (!(is.numeric(X))){
    stop("Input `clusters` should be a matrix of numbers")
  }
  scores <- c()
  K_max <- nrow(X)-1
  for (k in 2:min(10, K_max)){
    # call fit_assign to get vector of labels
    # need to unlist to get labels as vector
    labels <- unlist(fit_assign(X, k)[2])
    score <- avg_sil_score(labels, X)
    scores <- c(scores, score)
  }
  # use which.max to find index of greatest score
  # add 1 since starting from 2
  which.max(scores) + 1
}
