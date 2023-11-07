# Copyright 2023 Battelle Energy Alliance ALL RIGHTS RESERVED
data = test_regression_data_no_test()
y_params = list(
  mean = mean(data$y),
  sd = sd(data$y)
)
x_params = list(
  mean = apply(data$x, 2, mean),
  sd = apply(data$x, 2, sd)
)


test_that("normalize: verify the mean and sd are calculated for a vector", {
  test = normalize(data$y)
  expect_equal(test$norm_params, y_params)
})

test_that("normalize: verify the mean and sd are calculated for a matrix", {
  test = normalize(data$x)
  expect_equal(test$norm_params, x_params)
})

test_that("normalize: verify back transformation for a vector", {
  test = normalize(data$y)
  test_back = normalize(test$x, norm_params = test$norm_params)
  expect_equal(test_back$x, data$y)
})

test_that("normalize: verify back transformation for a matrix", {
  test = normalize(data$x)
  test_back = normalize(test$x, norm_params = test$norm_params)
  expect_equal(test_back$x, data$x)
})


