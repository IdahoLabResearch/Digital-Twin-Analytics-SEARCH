# Copyright 2023 Battelle Energy Alliance ALL RIGHTS RESERVED
data = test_regression_data_no_test()
fit = regLM(data)

test_that("regLM verify proper fit build", {
  expect_equal(class(fit), "regLM")
})


test_that("regLM predict verify list", {
  predicted = predict(fit, data)
  predicted_len = length(predicted$predicted)
  expect_equal(predicted_len, length(data$y))
})

test_that("regLM predict rmse", {
  predicted = predict(fit, data)
  pred_rmse = rmse(data$y, predicted$predicted)
  expect_lt(pred_rmse, 0.52)
})

