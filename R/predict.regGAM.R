#' Predict Generalized Linear Model
#'
#' Predict Generalized Linear Model.
#'
#' Copyright 2023 Battelle Energy Alliance ALL RIGHTS RESERVED
#' 
#' @inheritParams predict.regLM
#' @return predicted (list of scalar vectors) The predicted values of train and
#' test.
#'
#' @seealso predict.gam
#'
#' @export predict.regGAM
#' @export
predict.regGAM = function(object, x, ...) {
  # Back to original class.
  class(object) = object$orig_class

  # Obtain the predictions.
  y_hat = as.numeric(predict(object, newdata = x$x, ...))

  result = list(observed = x$y, predicted = y_hat)
  class(result) = "confirmReg"
  result
}


