# Copyright 2023 Battelle Energy Alliance ALL RIGHTS RESERVED
observed = rep(c(0,1,2), 5)
predicted = c(0, 2, 2, 0, 1, 1, 1, 0, 1, 0, 2, 0, 2, 2, 1)
class_0_tp = 3
class_2_tn = 6

test_that("confusionValues verify true positives and true negatives", {
  x = confusionValues(observed, predicted)
  expect_equal(c(x$`0`$tp, x$`2`$tn), c(class_0_tp, class_2_tn))
})
