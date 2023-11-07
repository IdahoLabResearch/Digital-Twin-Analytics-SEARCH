# Copyright 2023 Battelle Energy Alliance ALL RIGHTS RESERVED
data =  test_classification_data_no_test()

test_that("classLOGISTIC verify proper fit build", {
  fit = classLOGISTIC(data)
  expect_equal(class(fit), "regGLM")
})

test_that("classLOGISTIC prediction length", {
  fit = classLOGISTIC(data)
  predicted = predict(fit, data)
  predicted_len = length(predicted$predicted)

  expect_equal(predicted_len, length(data$y))
})

