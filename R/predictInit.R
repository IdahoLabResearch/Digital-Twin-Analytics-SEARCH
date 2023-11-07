#' Initialize Prediction
#'
#' Create an initial set of zeros for the train and test prediction.
#'
#' Copyright 2023 Battelle Energy Alliance ALL RIGHTS RESERVED
#'
#' @param train (data frame or matrix) A set of independent variables.
#' @param test (data frame or matrix) A set of independent variables for
#' test set.
#' @return predicted (list of scalar vectors) The model predicted.
#'
#' @export predictInit

predictInit = function(train, test = NULL) {
  # Initializing predicted values.
  predicted = list(
    train = rep(0, nrow(train)),
    test = rep(0, 1)
  )
  if (!is.null(test)) {
    predicted$test = rep(0, nrow(test))
  }

  predicted
}
