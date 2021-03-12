# test_find_elbow.R
# Tests for the find_elbow module

test_avg_sil_score <- function() {
  
  # make sure function raises error for bad input format (clusters)
  
  # make sure function raises error for bad input format (X)
  
  # generate some helper data
  helper_data <- array(c(c(0, 10, 10), c(0, 10, 11)), dim = c(3,2))
  helper_clusters <- c(1, 2, 2)
  
  # make sure function returns correct type
  expect_true(is.numeric(avg_sil_score(clusters, helper_data)))
  
  # Make sure function gives the correct score on helper data
  expect_true(all.equal.numeric(0.6207, avg_sil_score(clusters, helper_data), tolerance = 0.001))
}

test_avg_sil_score()