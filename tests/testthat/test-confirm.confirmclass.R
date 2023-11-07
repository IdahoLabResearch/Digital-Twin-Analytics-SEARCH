# Copyright 2023 Battelle Energy Alliance ALL RIGHTS RESERVED
x = list(
  observed = rep(c(0, 1, 2), 5),
  predicted = c(0, 2, 2, 0, 1, 1, 1, 0, 1, 0, 2, 0, 2, 2, 1)
)
class(x) = "confirmClass"
class_1_ppv = 0.2
class_2_npv = 0.6

test_that("confirm classification verify positive prediction and
          negative prediction values", {
  pred_summ = confirm(x)
  expect_equal(c(pred_summ$`1`$ppv, pred_summ$`2`$npv), c(class_1_ppv, class_2_npv))
})

