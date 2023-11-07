#' Predict Principal Component Regression
#'
#' Predict Principal Component Regression
#'
#' Copyright 2023 Battelle Energy Alliance ALL RIGHTS RESERVED
#' @param object (object) A fit object.
#' @param x (list) A list containing [x], the set of independent variables and
#' [y], the response.
#' @param ... Additional arguments.
#' @return predicted (list of scalar vectors) The observed and predicted values.
#'
#' @seealso predict.lm
#'
#' @export predict.regPCR
#' @export

predict.regPCR = function(object, x, ...) {
  # Back to original class.
  class(object) = object$orig_class
  # Obtain the predictions.
  proj = attr(object, "proj")
  newx = as.data.frame(as.matrix(x$x) %*% as.matrix(proj))

  # need to include two checks:
  # 1. Residual distance from the model plane
  # 2. Hotelling's T^2
  # see https://learnche.org/pid/latent-variable-modelling/principal-components-regression

  y_hat = as.numeric(predict(object, newdata = newx, ...))

  result = list(observed = x$y, predicted = y_hat)
  class(result) = "confirmReg"
  result
}
