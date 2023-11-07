# Copyright 2023 Battelle Energy Alliance ALL RIGHTS RESERVED
observed = 1:10
predicted = 2:11


test_that("Verify RMSE", {
  expect_equal(rmse(observed, predicted), 1)
})
