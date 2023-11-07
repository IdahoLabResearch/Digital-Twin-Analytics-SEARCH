#' Split Random
#'
#' Wrapper for splitting the data via random subset.
#'
#' Copyright 2023 Battelle Energy Alliance ALL RIGHTS RESERVED
#'
#' @param x (data frame or matrix) A set of independent variables.
#' @param train_percent (scalar | optional) The percent of values in the
#' training set.
#' @return split (list vector) The split of x into training and test set
#' indices.
#'
#' @export split_rand

split_rand = function(x, train_percent = 0.8) {
  n = dim(x)[1]
  k = floor(train_percent * n)
  train = sample(1:n, k)

  split = list(
    train = train,
    test = (1:n)[-train]
  )
  split
}
