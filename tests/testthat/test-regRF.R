# Copyright 2023 Battelle Energy Alliance ALL RIGHTS RESERVED
data = test_regression_data_no_test()
fit = regRF(data)

test_that("regRF verify proper fit build", {
  expect_equal(class(fit), "regRF")
})

test_that("regRF predict verify list", {
  predicted = predict(fit, data)
  predicted_len = length(predicted$predicted)
  expect_equal(predicted_len, length(data$y))
})

test_that("regRF predict rmse", {
  predicted = predict(fit, data)
  pred_rmse = rmse(data$y, predicted$predicted)
  expect_lt(pred_rmse, 0.23)
})
