# Copyright 2023 Battelle Energy Alliance ALL RIGHTS RESERVED
data = test_regression_data_no_test()
fit = regSVM(data)

test_that("regSVM verify proper fit build", {
  expect_equal(class(fit), "regSVM")
})


test_that("regSVM predict verify list", {
  predicted = predict(fit, data)
  predicted_len = length(predicted$predicted)
  expect_equal(predicted_len, length(data$y))
})

test_that("regSVM predict rmse", {
  predicted = predict(fit, data)
  pred_rmse = rmse(data$y, predicted$predicted)
  expect_lt(pred_rmse, 0.37)
})

