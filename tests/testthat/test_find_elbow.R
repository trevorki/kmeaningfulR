# test_find_elbow.R
# Tests for the find_elbow module

test_avg_sil_score <- function() {
  # generate some helper data
  helper_data <- array(c(c(0, 10, 10), c(0, 10, 11)), dim = c(3,2))
  helper_clusters <- c(1, 2, 2)
  
  # make sure function raises error for bad input format (clusters)
  expect_error(avg_sil_score('helper_clusters', helper_data))
  
  # make sure function raises error for bad input format (X)
  expect_error(avg_sil_score(helper_clusters, 'helper_data'))
  
  # make sure function returns correct type
  expect_true(is.numeric(avg_sil_score(clusters, helper_data)))
  
  # Make sure function gives the correct score on helper data
  expect_true(all.equal.numeric(0.6207, avg_sil_score(helper_clusters, helper_data), tolerance = 0.001))
}

test_find_elbow <- function() {
  # make sure function raises error for bad input format (X)
  expect_error(find_elbow('X'))
  
  # generate some helper data
  helper_data <- array(c(c(0, 10, 10, 10, 10), c(0, 10, 11, 0, 1)), dim = c(5,2))
  
  # make sure function returns correct type
  expect_true(length(find_elbow(helper_data))==1)
  expect_true(is.numeric(find_elbow(helper_data)))
  
  # Make sure function gives the correct value of K for the helper data clusters
  expect_true(find_elbow(helper_data)==3)
}

test_avg_sil_score()
test_find_elbow()