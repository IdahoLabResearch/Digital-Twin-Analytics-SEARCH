# Copyright 2023 Battelle Energy Alliance ALL RIGHTS RESERVED
data = test_regression_data_no_test()
fit = regPCR(data)

test_that("regPCR verify proper fit build", {
  expect_equal(class(fit), "regPCR")
})


test_that("regPCR predict verify list", {
  predicted = predict(fit, data)
  predicted_len = length(predicted$predicted)
  expect_equal(predicted_len, length(data$y))
})


test_that("regLM predict rmse", {
  predicted = predict(fit, data)
  pred_rmse = rmse(data$y, predicted$predicted)
  expect_lt(pred_rmse, 0.55)
})

