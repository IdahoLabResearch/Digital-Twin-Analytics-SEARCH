# Copyright 2023 Battelle Energy Alliance ALL RIGHTS RESERVED
data = test_regression_data_no_test()
fit = regENET(data)

test_that("regENET verify proper fit build", {
  expect_equal(class(fit), "regGLMNET")
})


test_that("regENET predict verify list", {
  predicted = predict(fit, data)
  predicted_len = length(predicted$predicted)
  expect_equal(predicted_len, length(data$y))
})

test_that("regENET predict rmse", {
  predicted = predict(fit, data)
  pred_rmse = rmse(data$y, predicted$predicted)
  expect_lt(pred_rmse, 0.6)
})



