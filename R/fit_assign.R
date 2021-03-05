# author: Trevor Kinsey
# date: 2021-03-05
#
# Module to find clusters in data using K-means
#



#' Chooses initial cluster locations using Kmeans++
#'
#' @param X array Data points of dimension (n,d)
#' @param k int The number of desired clusters
#'
#' @return array Initial coordinates of clusters
#' @export
#'
#' @examples
#' X = rbind(c(0,0), c(1,1))
#' init_centers(X, 2)
init_centers <- function(X, k){

}

#' Assigns data points to k clusters
#'
#' @param X array Data points of dimension (n,d)
#' @param centers array Coordinates of cluster centers, dimension (k,d)
#'
#' @return array Cluster assignments for each point in X, dimension (n,1)
#' @export
#'
#' @examples
#' X = rbind(c(0,0), c(1,1))
#' centers = rbind(c(0.1, 0.1), c(1.1, 1.1))
#' assign(X, centers)
assign <- function(X, centers){

}

#' Measures distance from data points to cluster centers
#'
#' @param X array Data points of dimension (n,d)
#' @param centers array Coordinates of cluster centers, dimension (k,d)
#'
#' @return array Distance from each point to each center, dimension (n,k)
#' @export
#'
#' @examples
#' X = rbind(c(0,0), c(1,1))
#' centers = rbind(c(0.1, 0.1), c(1.1, 1.1))
#' measure_dist(X, centers)
measure_dist <- function(X, centers){

}


#' Calculates center coordinates of each cluster
#'
#' @param X array Data points of dimension (n,d)
#' @param centers array Coordinates of previous cluster centers, dimension (k,d) (used only to determine number of centers k)
#' @param labels array Cluster assignments for each point in X, dimension (n,1)
#'
#' @return array Center coordinate for each cluster, dimension (k,d)
#' @export
#'
#' @examples
#' X = rbind(c(0,0), c(1,0), c(10,0), c(11,0))
#' centers = rbind(c(0, 0), c(10, 10))
#' labels = c(1, 1, 2, 2)
#' calc_centers(X, centers, labels)
calc_centers <- function(X, centers, labels){

}

#' Finds k clusters in data points.
#'
#' @param X array Data points of dimension (n,d)
#' @param k int The number of desired clusters
#'
#' @return array Coordinates of cluster centers, dimension (k,d)
#' @export
#'
#' @examples
#' X = rbind(c(0,0), c(1,1))
#' fit(X, 2)
fit <- function(X, k){

}

#' Finds k clusters in data points and assigns each point to a cluster.
#'
#' @param X array Data points of dimension (n,d)
#' @param k int The number of desired clusters
#'
#' @return list [1]: array of cluster centers, dimension(n,d)
#'              [2]: array of cluster labels, dimension (n,1)
#' @export
#'
#' @examples
#' X = rbind(c(0,0), c(1,1))
#' fit_assign(X, 2)
fit_assign <- function(X, k){

}


