#' @export
confirm.default = function(x, ...) {
  x$observed == x$predicted
}

#' Create a set of prediction summary values based on the class.
#'
#' Copyright 2023 Battelle Energy Alliance ALL RIGHTS RESERVED
#'
#' @param x (list of vectors) A list containing the observed and predicted
#' values.
#' @param ... Additional parameters
#' @return predict_summary (list) The summary prediction values.
#'
#' @export
#' @export confirm
confirm = function(x, ...)
  UseMethod("confirm")
