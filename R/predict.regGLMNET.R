#' Predict function for all glmnet models
#'
#' Prediction for glmnet regression models including: enet, lasso,
#' and ridge.
#'
#' Copyright 2023 Battelle Energy Alliance ALL RIGHTS RESERVED
#'
#' @inheritParams predict.regLM
#' @return predicted (list of scalar vectors) The predicted values of train and
#' test.
#'
#' @seealso predict.glmnet
#'
#' @export predict.regGLMNET
#' @export
predict.regGLMNET = function(object, x, ...) {
  # Back to original class.
  class(object) = object$orig_class

    # Obtain the predictions.
  y_hat = as.numeric(predict(object, newx = as.matrix(x$x), ...))

  result = list(observed = x$y, predicted = y_hat)
  class(result) = "confirmReg"
  result
}
