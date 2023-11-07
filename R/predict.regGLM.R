#' Predict regBinomial
#'
#' Create an initial set of zeros for the train and test prediction.
#'
#' Copyright 2023 Battelle Energy Alliance ALL RIGHTS RESERVED
#'
#' @inheritParams predict.regLM
#' @return predicted (list of scalar vectors) The predicted values of train and
#' test.
#'
#' @seealso predict.glm
#'
#' @export predict.regGLM
#' @export
predict.regGLM = function(object, x, ...) {
  # Back to original class.
  class(object) = object$orig_class

  # Obtain the predictions.
  y_hat = predict(object, x$x, ...)

  result = list(observed = x$y, predicted = y_hat)
  class(result) = "confirmReg"
  result
}
