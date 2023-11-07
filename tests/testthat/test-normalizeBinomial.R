# Copyright 2023 Battelle Energy Alliance ALL RIGHTS RESERVED
data = test_regression_data_no_test()
norm_data = normalizeBinomial(data$y)

test_that("normalizeBinomial verify max", {
  expect_equal(max(norm_data$x), 1)
})

test_that("normalizeBinomial verify min", {
  expect_gt(min(norm_data$x), 0)
})

test_that("normalizeBinomial verify reverse transform", {
  expect_equal(data$y, normalizeBinomial(norm_data$x,
                                        norm_params = norm_data$norm_params)$x)
})

