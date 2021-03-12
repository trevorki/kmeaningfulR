# author: Trevor Kinsey
# date: 2021-03-05
#
# Module to find clusters in data using K-means
#

library(tidyverse)


#' Makes clusters of n points in d dimensions
#' @param n int Number of points
#' @param k int Number of clusters
#' @param d int Number of dimensions
#'
#' @return Dataframe of points
#' @export
#'
#' @examples
#' df = make_blobs(n, k, d)
#'
make_blobs <- function(n, k, d) {
  blob_counts <- rep(0, k)                    #
  for (nn in 1:n){                            # figure out how many points in each blob
    blob_counts[nn%%k + 1] <- blob_counts[nn%%k + 1] + 1
  }
  centers = matrix(9*runif(k*d)+0.5 ,ncol=d)  # initialize random centers between 0.5 and 9.5 in each dimension
  X = matrix(0, n, d)
  start_index <- 0

  for (kk in 1:k){                         # for every blob center
    len <- blob_counts[kk]                 # number of points in current blob
    for (l in 1:len){                      # for every point in this blob
      X[start_index + l,] <- centers[kk,]  # set blob's points to center coordinates
    }
    start_index <- start_index + len       # point to start of next blob
  }
  X <- X + matrix(rep(rnorm(n*d,0,0.5)),n)  # add noise to centers
  X
}


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



n <- 100
k <- 3
d <- 2
X = make_blobs(n, k, d)

#plot_blobs()

X_df <- as.data.frame(X)
ggplot(X_df, aes(x = V1, y = V2))+
  geom_point() +
  scale_x_continuous(limits = c(0,10),breaks = c(0,2,4,6,8,10)) +
  scale_y_continuous(limits = c(0,10),breaks = c(0,2,4,6,8,10))

