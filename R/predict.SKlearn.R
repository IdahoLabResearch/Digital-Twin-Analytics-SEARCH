#' Predict regMLP
#'
#' Create an initial set of zeros for the train and test prediction.
#'
#' Copyright 2023 Battelle Energy Alliance ALL RIGHTS RESERVED
#'
#' @inheritParams predict.regLM
#' @return predicted (list of scalar vectors) The predicted values of train and
#' test.
#'
#' @import reticulate
#'
#' @export predict.regMLP
#' @export
predict.regMLP = function(object, x, ...) {
  py_x = r_to_py(x$x)

  # Obtain the predictions.
  y_hat = as.numeric(unlist(object$predict(py_x)))

  result = list(observed = x$y, predicted = y_hat)
  class(result) = "confirmReg"
  result
}
