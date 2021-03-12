# author: Trevor Kinsey
# date: 2021-03-12
#
# Tests for fit_assign.R
#

################ init_centers ###################

# reject if there are not more points than centers
X <- array(0,c(2,2))
expect_error(init_centers(X, 2))
# reject if k < 2
expect_error(init_centers(X, 1))

# centers are initialised to different points
X <- rbind(c(0,0), c(1,1), c(2,2))
centers <- init_centers(X,2)
expect_false(isTRUE(all.equal(centers[1,], centers[2,])))
# the correct number of centers are created
expect_equal(dim(init_centers(X,2))[1], 2)
# the centers have the same width as the data
expect_equal(dim(init_centers(X,2)[2]), 2)

################ measure_dist ##################

# Throw error if there are not more points than centers
centers <- rbind(c(0,0), c(1,1), c(2,2))
expect_error(measure_dist(X,centers))

# check that it calculates 2-d distance correctly
X <- array(c(0,0), c(1,2))
center <- array(c(3,4), c(1,2))
expect_equal(measure_dist(X, center), 5)
# check that it calculates distance between same point is zero
expect_equal(measure_dist(X, X), 0)

################### assign ######################

# throw error if `X` and `centers` have different widths
X <- array(1, c(3,2))
centers <- array(1, c(2,3))
expect_error(assign(X, centers))

# check that is returns the right label
X <- rbind(c(0,0), c(10,0))
centers <- rbind(c(1,0), c(9,0))
expect_true(all(assign(X, centers)==c(1,2)))
#
