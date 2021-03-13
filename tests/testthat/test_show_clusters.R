# test_show_clusters.R

test_that('plot should use geom_point and map x to x-ais, and y to y-axis.',{

  X <- matrix(c(-1,0,1,0,0,1,0,-1), 4, 2, byrow = TRUE)
  centers <- array(0,c(1,2))
  labels <- c(1,1,1,1)
  plot <- show_clusters(X, labels,centers)

  expect_true("GeomPoint" %in% class(plot$layers[[1]]$geom)[1])
  expect_true("GeomPoint" %in% class(plot$layers[[2]]$geom)[1])
  expect_true(rlang::get_expr(plot$layers[[1]]$mapping$x) == 'Dim.1')
  expect_true(rlang::get_expr(plot$layers[[2]]$mapping$x) == 'Dim.1')
  expect_true(rlang::get_expr(plot$layers[[1]]$mapping$y) == 'Dim.2')
  expect_true(rlang::get_expr(plot$layers[[2]]$mapping$y) == 'Dim.2')
  expect_true(rlang::get_expr(plot$layers[[1]]$mapping$colour) == 'clusters')
  expect_true(rlang::get_expr(plot$layers[[2]]$mapping$colour) == 'clusters')

})
