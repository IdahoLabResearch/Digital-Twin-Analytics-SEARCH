#' Split Sequential
#'
#' Wrapper for splitting the data via a sequence of values used in forecast
#' methods.
#'
#' Copyright 2023 Battelle Energy Alliance ALL RIGHTS RESERVED
#'
#' @param x (data frame or matrix) A set of independent variables.
#' @param train_percent (scalar | optional) The percent of values in the
#' training set.
#' @return split (list vector) The split of x into training and test set
#' indices.
#'
#' @export split_seq

split_seq = function(x, train_percent = 0.8) {
  n = dim(x)[1]
  k = floor(train_percent * n)

  split = list(
    train = 1:k,
    test = (k + 1):n
  )
  split
}
