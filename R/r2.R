#' r2
#'
#' Coefficient of determination
#'
#' Copyright 2023 Battelle Energy Alliance ALL RIGHTS RESERVED
#'
#' @inheritParams rmse
#' @return r2 (scalar float) The r2 between the observed and predicted.
#'
#' @export r2

r2 = function(observed, predicted) {
  numerator = sum((observed - predicted)^2)
  denominator = sum((observed - mean(observed))^2)
  1 - numerator / denominator
}
