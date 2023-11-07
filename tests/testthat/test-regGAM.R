# Copyright 2023 Battelle Energy Alliance ALL RIGHTS RESERVED
data = test_regression_data_no_test()
fit = regGAM(data)

test_that("regGAM verify proper fit build", {
  expect_equal(class(fit), "regGAM")
})

test_that("predict.regGAM prediction length", {
  predicted = predict(fit, data)
  predicted_len = length(predicted$predicted)

  expect_equal(predicted_len, length(data$y))
})

test_that("predict.regGAM rmse", {
  predicted = predict(fit, data)
  predicted_rmse = rmse(data$y, predicted$predicted)
  expect_lt(predicted_rmse, .46)
})

