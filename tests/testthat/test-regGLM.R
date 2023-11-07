# Copyright 2023 Battelle Energy Alliance ALL RIGHTS RESERVED
data = test_regression_data_no_test()
y_non = data$y
y_normalized = normalizeBinomial(data$y)$x
data$y = y_normalized
fit = regGLM(data, family = "quasibinomial")

test_that("regGLM verify proper fit build", {
  expect_equal(class(fit), "regGLM")
})


test_that("predict regGLM prediction length", {
  data$y = y_normalized
  predicted = predict(fit, data)
  predicted_len = length(predicted$predicted)

  expect_equal(predicted_len, length(data$y))
})

test_that("predict regGLM rmse", {
  predicted = predict(fit, data)
  predicted_rmse = rmse(data$y, predicted$predicted)
  expect_lt(predicted_rmse, 1)
})

