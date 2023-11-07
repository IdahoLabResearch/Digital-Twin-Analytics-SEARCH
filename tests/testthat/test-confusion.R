# Copyright 2023 Battelle Energy Alliance ALL RIGHTS RESERVED
observed = rep(c(0,1,2), 5)
predicted = c(0, 2, 2, 0, 1, 1, 1, 0, 1, 0, 2, 0, 2, 2, 1)

test_that("confusion check the diagonal", {
  metric = as.matrix(confusion(observed, predicted))
  expect_equal(sum(diag(metric)), 5)
})
