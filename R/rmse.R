#' rmse
#'
#' Root mean square error.
#'
#' Copyright 2023 Battelle Energy Alliance ALL RIGHTS RESERVED
#'
#' @param observed (scalar vector) The observed values for the response.
#' @param predicted (scalar vector) The fitted values developed by the model.
#' @return rmse
#'
#' @export rmse

rmse = function(observed, predicted) {
  sqrt(mean((observed - predicted)^2))
}
