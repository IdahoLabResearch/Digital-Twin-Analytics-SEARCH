#' Remove time series outliers
#'
#' Remove time series outliers. Each column is assumed to be a time series.
#'
#' Copyright 2023 Battelle Energy Alliance ALL RIGHTS RESERVED
#'
#' @param x (matrix) A matrix of values in which there are suspected outliers.
#' @return fit (model specific object) The model fit.
#'
#' @importFrom forecast tsclean
#' @export exploreTSoutliers
exploreTSoutliers = function(x) {
  # loop through each column and remove any outliers.
  for (i in 1:ncol(x)) {
    x[, i] = tsclean(x[, i])
  }

  x
}
