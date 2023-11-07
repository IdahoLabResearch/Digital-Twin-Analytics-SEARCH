#' Linear regression
#'
#' Wrapper for linear regression.
#'
#' Copyright 2023 Battelle Energy Alliance ALL RIGHTS RESERVED
#'
#' @param object (object) A fit object.
#' @param x (list) A list containing [x], the set of independent variables and
#' [y], the response.
#' @param ... Additional arguments.
#' @return predicted (list of scalar vectors) The observed and predicted values.
#'
#' @seealso predict.lm
#'
#' @export predict.regLM
#' @export

predict.regLM = function(object, x, ...) {
  # Back to original class.
  class(object) = object$orig_class
  # Obtain the predictions.
  y_hat = as.numeric(predict(object, newdata = x$x, ...))

  result = list(observed = x$y, predicted = y_hat)
  class(result) = "confirmReg"
  result
}
