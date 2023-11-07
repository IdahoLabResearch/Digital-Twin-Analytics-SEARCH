#' mape
#'
#' Mean absolute percent error.
#'
#' Copyright 2023 Battelle Energy Alliance ALL RIGHTS RESERVED
#'
#' @inheritParams rmse
#' @return mape (scalar float) The mape between the observed and predicted.
#'
#' @export mape

mape = function(observed, predicted) {
  result = rep(0, length(observed))
  zeros = which(observed == 0)
  # cannot divide by zero
  if (length(zeros) != 0) {
    observed[zeros] = 1
    result = mean(abs((observed - predicted) / observed))
    result[zeros] = 999
  } else {
    result = mean(abs((observed - predicted) / observed))
  }

  result
}
