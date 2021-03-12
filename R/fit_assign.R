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

  # Throw error if more centers than data points
  if(dim(X)[1] <= k){
    stop("There must be more data points than centers")
  }
  # Throw error if k f< 2
  if(k < 2){
    stop("There must be at least 2 clusters")
  }



  n <- dim(X)[1]
  d <- dim(X)[2]
  centers <- matrix(0, k, d)
  ind <- list()

  # pick 1st center at random
  ind <- append(ind, sample(1:n,1))
  centers[1,] <- X[ind[[1]],]

  for (kk in 2:k) {      # for every center measure distance from every point
    dists_sq <- measure_dist(X, matrix(centers[1:kk,], ncol = d))
    for (i in ind){
      dists_sq[i,] <- 10^10   # set distance between existing centers to very large
    }
    dists_sq <- apply(dists_sq, MARGIN = 1,FUN = min) # keep only the smallest distance for each point
    dists_sq[dists_sq == 10^10] <- 0   # set distance between existing centers to zero
    probs <- dists_sq / sum(dists_sq)
    ind <- append(ind, sample(length(probs), 1, prob = probs))
    centers[kk,] <- X[ind[[length(ind)]],]
  }
  centers
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

  # Throw error if more centers than data points
  if(dim(X)[1] < dim(centers)[1]){
    stop("There must be more data points than centers")
  }

  k <- dim(centers)[1]
  n <- dim(X)[1]

  distances = matrix(0, n, k)
  for (kk in 1:k){
    for (nn in 1:n){
      pt <- X[nn,]
      cent <- centers[kk,]
      distances[nn,kk] <- sqrt(sum((pt-cent)^2))
    }
  }
  distances
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

  # Throw error if X and centers have different widths
  if(dim(X)[2] < dim(centers)[2]){
    stop("`X` and `centers` must have the same width")
  }


  n <- dim(X)[1]
  k <- dim(centers)[1]
  labels <- matrix(0,n,1)
  distances <- measure_dist(X, centers)
  for (nn in 1:n){
    labels[nn] <- which.min(distances[nn,])
  }
  labels
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

  # Throw error if `X` and `labels` have different lengths
  if(dim(X)[1] != length(labels)){
    stop("There must not the same number of labels as points")
  }
  # Throw error if X and centers have different widths
  if(dim(X)[2] < dim(centers)[2]){
    stop("`X` and `centers` must have the same width")
  }


  n <- dim(X)[1]
  d <- dim(X)[2]
  k <- dim(centers)[1]

  new_centers <- matrix(0, k, d)
  for (kk in 1:k){
    # if the current center has points assigned to it take the mean of points
    if (dim(matrix(X[labels == kk,], ncol = d))[1] > 1){
      current_center <- matrix(apply(X[labels == kk,], MARGIN = 2,FUN = mean),
                               ncol=d)
      new_centers[kk,] <- current_center
    }
    else {    #set the new center to the point farthest from current center
      dists <- measure_dist(X, matrix(centers[kk,], ncol = d))
      new_centers[kk,] <- X[which.max(dists),]
    }
  }
  new_centers
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

  # Throw error if X contains missing values
  if(any(is.na(X))){
    stop("`X` contains missing values")
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

  centers <- init_centers(X, k)

  labels <- assign(X,centers)
  new_centers <- calc_centers(X, centers, labels)
  new_labels <- assign(X, centers)

  i <- 1
  while(centers - new_centers && i < 30){
    centers <- new_centers
    labels <- new_labels
    new_labels <- assign(X, centers) # assign cluster label based on closest center
    new_centers <- calc_centers(X, centers, new_labels)
    i <- i + 1
  }
  new_centers
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

  # Throw error if X contains missing values
  if(any(is.na(X))){
    stop("`X` contains missing values")
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

  centers = fit(X, k)
  labels = assign(X, centers)
  list(centers, labels)
}


