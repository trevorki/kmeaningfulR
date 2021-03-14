library(FactoMineR)
library(forcats)
library(ggplot2)
library(dplyr)

# author: Hazel Jiang
# date: 2021-03-06
#
# Module to plot clusters by colour
#

#' This function reduces a data set to 2 dimensions using principle component
#' analysis (PCA) and colours clusters of points.
#'
#' @param X array Data points of dimension (n,d)
#' @param clusters array Cluster assignments for each point in X,
#' dimension (n,1)
#' @param centroids array Coordinates of cluster centers, dimension (k,d)
#'
#' @return plot A 2d principle components scatter plot coloured by cluster
#' @export
#'
#' @examples
#' X = rbind(c(0,0), c(1,0), c(10,0), c(11,0))
#' centers = rbind(c(0, 0), c(10, 10))
#' labels <- c(1,1,1,1)
#' show_clusters(X, labels,centers)
show_clusters <- function(X, clusters, centroids){

  # Throw error if X and centers have different widths
  if(dim(X)[2] != dim(centroids)[2]){
    stop("`X` and `centers` must have the same width")
  }

  # Throw error if `X` and `labels` have different lengths
  if(dim(X)[1] != length(clusters)){
    stop("There must the same number of labels as points")
  }

  row_x <- nrow(X)
  combine_df <- rbind(X, centroids)
  pca = PCA(combine_df, scale.unit = TRUE, ncp = 2, graph = FALSE)
  transformed_data <- as.data.frame(pca$ind$coord)
  transformed_X <- transformed_data[1:row_x,]
  transformed_centroid <- slice(transformed_data,
                                (row_x+1):nrow(transformed_data))
  transformed_X$clusters = as.factor(clusters)
  transformed_centroid$clusters = as.factor(1:dim(centroids)[1])

  plot <- ggplot() +
    geom_point(data = transformed_X, aes(Dim.1, Dim.2, colour = clusters),
               size = 1, alpha = 0.7) +
    geom_point(data = transformed_centroid, aes(Dim.1, Dim.2,
                                                colour = clusters),
               size = 4, shape = 8) +
    ggtitle('PCA Plot') +
    theme_bw()

  plot
}
