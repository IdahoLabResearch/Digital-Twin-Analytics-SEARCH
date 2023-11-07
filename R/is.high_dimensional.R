#' Is the data high dimensional
#'
#' Utils function to check if high-dimensional $(n < p)$, i.e., the number of
#' rows are far less than the number of parameters.
#'
#' Copyright 2023 Battelle Energy Alliance ALL RIGHTS RESERVED
#'
#' @param x (data frame or matrix) A set of independent variables.
#' @return is.low (bool) Is x low dimensional.
#'
#' @export is.high_dimensional

is.high_dimensional = function(x) {
  return(dim(x)[1] <= dim(x)[2] | dim(x)[2] >= 50)
}
