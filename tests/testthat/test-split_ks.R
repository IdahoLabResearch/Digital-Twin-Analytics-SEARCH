# Copyright 2023 Battelle Energy Alliance ALL RIGHTS RESERVED
data = test_regression_data_no_test()

test_that("Verify split_ks length of train: test less than train", {
  split = split_ks(data$x, 0.8)
  train_len = length(split$train)

  expect_equal(train_len, 142)
})

test_that("Verify split_ks length of train: test less than train", {
  split = split_ks(data$x, 0.8)
  test_len = length(split$test)

  expect_equal(test_len, 36)
})

test_that("Verify split_ks length of train: test greater than train", {
  split = split_ks(data$x, 0.2)
  train_len = length(split$train)

  expect_equal(train_len, 35)
})

test_that("Verify split_ks length of train: test greater than train", {
  split = split_ks(data$x, 0.2)
  test_len = length(split$test)

  expect_equal(test_len, 143)
})
