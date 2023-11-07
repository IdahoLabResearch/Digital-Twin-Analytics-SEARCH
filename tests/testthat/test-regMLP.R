# Copyright 2023 Battelle Energy Alliance ALL RIGHTS RESERVED
data = test_regression_data_no_test()
# fit = regMLP(data)
#
# test_that("regMLP verify proper fit build", {
#   expect_equal(class(fit)[1], "regMLP")
# })
#
#
# test_that("regMLP predict verify list", {
#   predicted = predict(fit, data)
#   predicted_len = length(predicted$predicted)
#   expect_equal(predicted_len, 2)
# })
#
# test_that("regMLP predict rmse", {
#   predicted = predict(fit, data)
#   pred_rmse = rmse(data$y, predicted$predicted)
#   expect_lt(pred_rmse, 1.0)
# })
