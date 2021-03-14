# author: Sasha Babicki
# date: 2021-03-11
#
# Tests for preprocess.R
#

# empty dataframe, array, or matrix should throw exception
expect_error(preprocess(data.frame()))
expect_error(preprocess(array()))
expect_error(preprocess(matrix(nrow=1, ncol = 1)))

# non-array-like data should throw an exception
expect_error(preprocess("string"))
expect_error(preprocess(TRUE))

# Possibly update this in the future to process text data
# For now the R version of the package does not handle non-numeric data

# reject non-numeric data
expect_error(preprocess(data.frame("text", "data")))

# array with one col and one row 0 should be the same scaled or not
X <- data.frame(0)
expected_output <- as.matrix(X)
expect_equal(preprocess(X), expected_output)

# return type of processed data should be dataframe
expect_is(preprocess(X), c("data.frame", "matrix", "array"))

# dataframe with two columns with same values should be [0, 0]
X <- data.frame(1,1)
expected_output <- array(data.frame(0, 0))
expect_true(all(preprocess(X)==expected_output))

# dataframe with two rows with same values should be [0, 0]
X <- data.frame(c(2, 2))
expected_output <- array(data.frame(c(0, 0)))
expect_true(all(preprocess(X) == expected_output))

## imputation is working as expected
X <- matrix(1,10,5)
X[9,3] <- NA  # set some random cells to NA
X[1,2] <- NA
expect_true(any(sapply(X, is.na)))  # check that test code working
expect_false(any(sapply(preprocess(X), is.na)))  # result should not have nans

# handle missing data with imputation
X <- data.frame(c(0, NA))
expected_output <- as.matrix(data.frame(c(0, 0))) # fill with mean and then scale
expect_true(all(preprocess(X)==expected_output))

# reject when all data missing
X <- data.frame(c(c(NA, NA), c(NA, NA)))
expect_error(preprocess(X))

