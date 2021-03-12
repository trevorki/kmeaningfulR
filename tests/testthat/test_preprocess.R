# test_preprocess.R
# Tests for preprocess.R

# empty dataframe, array, or matrix should throw exception
expect_error(preprocess(data.frame()))
expect_error(preprocess(array()))
expect_error(preprocess(matrix(nrow=1, ncol = 1)))

# non-array-like data should throw an exception
expect_error(preprocess("string"))
expect_error(preprocess(TRUE))

# array with one col and one row 0 should be the same scaled or not
X <- data.frame(0)
expected_output <- array(X)
expect_equal(preprocess(X), expected_output)

# return type of processed data should be dataframe
expect_is(preprocess(X), c("data.frame", "matrix", "array"))

# dataframe with two columns with same values should be [0, 0]
X <- data.frame(1,1)
expected_output <- array(data.frame(0, 0))
expect_equal(preprocess(X), expected_output)

# dataframe with two rows with same values should be [0, 0]
X <- data.frame(c(2, 2))
expected_output <- array(data.frame(c(0, 0)))
expect_equal(preprocess(X), expected_output)

## imputation is working as expected
#X = make_blobs(n_samples=10, centers=3, n_features=2)
#mask = np.random.choice([True, False], size=X.shape)
#X[mask] = None  # set entries as None at random
#assert np.isnan(X).any()  # check that test code working
#assert not np.isnan(preprocess(X)).any()  # result should not have nans
#
## handle missing data with imputation
#X = [[None], [1]]
#expected_output = np.array([[0., 0.]])  # fill with mean and then scale
#assert (preprocess(X) == expected_output).all()

# reject when all data missing
X = data.frame(c(c(NULL, NULL), c(NULL, NULL)))
expect_error(preprocess(X))

## use correct one-hot-encoding for categorical data
#X = [["neutral", "large"], ["neutral", "medium"]]
#expected_output = np.array([[1., 1., 0.], [1., 0., 1.]])
#assert (preprocess(X) == expected_output).all()
#
## use combination of scaling and OHE for combo data
#X = [[2, "medium", "neutral"], [None, None, "neutral"]]
#expected_output = np.array([[0., 0., 1., 1.], [0., 1., 0., 1.]])
#assert (preprocess(X) == expected_output).all()
#
## possibly update in future to use
## BOW encoding for other non-numeric data
## e.g. length > 20 and no repeated words in col
#
