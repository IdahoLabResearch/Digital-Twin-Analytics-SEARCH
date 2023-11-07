#' Predict try error
#'
#' Returns a set of zeros.
#'
#' Copyright 2023 Battelle Energy Alliance ALL RIGHTS RESERVED
#'
#' @inheritParams predict.regLM
#' @return predicted (list of scalar vectors) The predicted values of train and
#' test.
#'
#'
#' @export predict.try_error
#' @export
predict.try_error = function(object, x, ...) {
  result = list(observed = x$y, predicted = rep(0, length(x$y)))
  class(result) = "confirmReg"
  result
}
