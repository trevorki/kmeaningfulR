# author: Trevor Kinsey
# date: 2021-03-12
#
# Tests for fit_assign.R
#


################ init_centers ###################

test_that("Throw error if there are not more points than centers",{
  X <- array(0,c(2,2))
  expect_error(init_centers(X, 3))
})

test_that("Throw error if k is less than 2",{
expect_error(init_centers(X, 1))
})

test_that("Centers are not assigned to the same point",{
  X <- rbind(c(0,0), c(1,1), c(2,2))
  centers <- init_centers(X,2)
  expect_false(isTRUE(all.equal(centers[1,], centers[2,])))
})

test_that("The correct number of centers are created",{
  X <- rbind(c(0,0), c(1,1), c(2,2))
  expect_equal(dim(init_centers(X,2))[1], 2)
})

test_that("centers and X have the same width",{
  X <- rbind(c(0,0), c(1,1), c(2,2))
  expect_equal(dim(init_centers(X,2))[2], 2)
})

# ################ measure_dist ##################

test_that("Throw error if there are more centers than points",{
  X <- rbind(c(0,0), c(1,1))
  centers <- rbind(c(0,0), c(1,1), c(2,2))
  expect_error(measure_dist(X,centers))
})

test_that("2D distance is correctly calculated",{
  X <- array(c(0,0), c(1,2))
  center <- array(c(3,4), c(1,2))
  expect_equal(measure_dist(X, center), array(5, c(1,1)))
})

test_that("The distance between a point and itself is zero",{
  X <- array(c(0,0), c(1,2))
  expect_equal(measure_dist(X, X), array(0, c(1,1)))
})

# ################### assign ######################

test_that("Throw error if `X` and `centers` have different widths",{
  X <- array(1, c(3,2))
  centers <- array(1, c(2,3))
  expect_error(assign(X, centers))
})

test_that("The correct label is returned",{
  X <- rbind(c(0,0), c(10,0))
  centers <- rbind(c(1,0), c(9,0))
  expect_true(all(assign(X, centers)==c(1,2)))
})

test_that("A center is ignored if not points are assigned to it",{
  X <- array(c(1,2,3,4), c(4,1))
  centers <- array(c(1,10), c(2,1))
  expect_equal(assign(X, centers), array(c(1,1,1,1), c(4,1)))
})

test_that("Points equidistant to 2 centers are assigned to the first one",{
  X <- array(c(5,5), c(2,1))
  centers <- array(c(1,10), c(2,1))
  expect_equal(assign(X, centers), array(c(1), c(2,1)))
})

###################### calc_centers ################

test_that("Throw an error if different number of data points and labels",{
  X  <- array(1, c(10,2))
  centers <- array(1, c(2,2))
  labels <- array(1, c(9,1))
  expect_error(calc_centers(X, centers, labels))
})

test_that("One center is correctly calculated",{
  X <- rbind(c(-1,0),c(1,0),c(0,1),c(0,-1))
  centers <- array(1000, c(1,2))
  labels <- array(1, c(4,1))
  expect_equal(calc_centers(X,centers,labels), array(0, c(1,2)))
})

test_that("Two centers are correctly calculated",{
  X <- rbind(c(1,0),c(-1,0),c(9,0),c(11,0))
  centers <- rbind(c(0,0),c(10,0))
  labels <- array(c(1,1,2,2))
  expect_equal(calc_centers(X,centers,labels), rbind(c(0,0),c(10,0)))
})

test_that("If one center is the same as another center it gets moved",{
  X <- rbind(c(1,0),c(-1,0),c(9,0),c(11,0),c(19,0),c(21,0))
  centers <- rbind(c(0,0),c(100,0), c(200,0))
  labels <- array(c(0,0,0,0,0,0))
  new_centers <- rbind(c(21,0),c(-1,0), c(-1,0))
  expect_equal(calc_centers(X, centers, labels), new_centers)
})

############### fit ##########################

test_that("Throw error if `X` has missing values",{
  X <- array(NA, c(2,4))
  expect_error(fit(X,2))
})

test_that("throw an error if X is not array-like",{
  expect_error(fit(5,2))
  expect_error(fit("Hello" ,2))
})

test_that("four points are correctly assigned to four clusters",{
  X <- rbind(c(0,0),c(0,2),c(10,0),c(10,2))
  expected_centers <- c(c(0,0),c(0,2),c(10,0),c(10,2))
  centers <- fit(X, 4)
  expect_true(all(centers[1,] %in% expected_centers))
  expect_true(all(centers[1,] %in% expected_centers))
  expect_true(all(centers[1,] %in% expected_centers))
  expect_true(all(centers[1,] %in% expected_centers))
})

test_that("Four points are correctly assigned to 2 clusters",{
  X <- rbind(c(0,0),c(0,2),c(100,0),c(100,2))
  centers = fit(X, 2)
  expect_false(centers[1,1] == centers[2,1])
})

############ fit assign ####################
test_that("four points are assigned to four clusters with unique labels",{
  X <- rbind(c(0,0),c(0,2),c(10,0),c(10,2))
  expected_centers <- c(c(0,0),c(0,2),c(10,0),c(10,2))
  km <- fit_assign(X, 4)
  centers <- km[[1]]
  labels <- km[[2]]
  expect_true(all(centers[1,] %in% expected_centers))
  expect_true(all(centers[1,] %in% expected_centers))
  expect_true(all(centers[1,] %in% expected_centers))
  expect_true(all(centers[1,] %in% expected_centers))
  expect_equal(length(unique(labels)), 4)
})
