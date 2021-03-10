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
  blob_counts <- rep(0, k)                    # initialize with zero points per blob
  for (nn in 1:n){                            # figure out how many points in each blob
    blob_counts[nn%%k + 1] <- blob_counts[nn%%k + 1] + 1
  }
  centers = matrix(9*runif(k*d)+0.5 ,ncol=d)  # initialize random centers between 0.5 and 9.5 in each dimension
  X = matrix(0, n, d)                      # initialize empty matrix
  start_index <- 0                         # start with 0th point

  for (kk in 1:k){                         # for every blob center
    len <- blob_counts[kk]                 # number of points in current blob
    for (l in 1:len){                      # for every point in this blob
      X[start_index + l,] <- centers[kk,]  # set blob's points to center coordinates
    }
    start_index <- start_index + len       # point to start of next blob
  }
  X + matrix(rep(rnorm(n*d,0,0.5)),n)     # add noise to centers
}
