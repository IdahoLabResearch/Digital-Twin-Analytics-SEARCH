#' Split KS
#'
#' Wrapper for splitting the data via Kennard-Stone algorithm.
#'
#' Copyright 2023 Battelle Energy Alliance ALL RIGHTS RESERVED
#'
#' @param x (data frame or matrix) A set of independent variables.
#' @param train_percent (scalar | optional) The percent of values in the
#' training set.
#' @return split (list vector) The split of x into training and test set
#' indices.
#'
#' @import prospectr
#' @export split_ks

split_ks = function(x, train_percent = 0.8) {
  n = dim(x)[1]
  k = floor(train_percent * n)

  sets = kenStone(x, k)
  split = list(train = sets$model, test = sets$test)
  split
}
