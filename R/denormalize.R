#' Denormalize
#'
#' Multiply by the standard deviation and add the mean.
#'
#' Copyright 2023 Battelle Energy Alliance ALL RIGHTS RESERVED
#'
#' @param x (matrix, data frame, or scalar vector) The numeric values to be scaled.
#' @param x_mean (scalar vector or float) The column means of x.
#' @param x_sd (scalar vector or float) The column standard deviations of x.
#' @return x (matrix, data frame, or scalar vector) The descaled values of x.
#'
#' @export denormalize

denormalize = function(x, x_mean, x_sd) {
  # Check if the value is a vector or matrix.
  if ( is.null(dim(x)) ) {
    x = x * x_sd + x_mean
  } else {
    x = sweep(sweep(x, 2, x_sd, "*"), 2, x_mean, "+")
  }

  x
}
