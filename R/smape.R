#' smape
#'
#' Symmetrical mean absolute percent error
#'
#' Copyright 2023 Battelle Energy Alliance ALL RIGHTS RESERVED
#'
#' @inheritParams rmse
#' @return smape
#'
#' @export smape

smape = function(observed, predicted) {
  numerator = abs(predicted - observed)
  denominator = (abs(observed) + abs(predicted)) / 2

  result = rep(0, length(observed))
  zeros = which(denominator == 0)
  # cannot divide by zero
  if (length(zeros) != 0) {
    denominator[zeros] = 1
    result = mean(numerator / denominator)
    result[zeros] = 999
  } else {
    result = mean(numerator / denominator)
  }

  result
}
