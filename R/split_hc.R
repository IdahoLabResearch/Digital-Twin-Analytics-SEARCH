#' Split Hierarchical
#'
#' Wrapper for splitting the data via Hierarchical Clustering.
#'
#' Copyright 2023 Battelle Energy Alliance ALL RIGHTS RESERVED
#'
#' @param x (data frame or matrix) A set of independent variables.
#' @param train_percent (scalar | optional) The percent of values in the
#' training set.
#' @return split (list vector) The split of x into training and test set
#' indices.
#'
#' @importFrom stats dist
#' @import protoclust
#' @export split_hc

split_hc = function(x, train_percent = 0.8) {
  n = dim(x)[1]
  k = floor(train_percent * n)
  test_k = n - k

  dendro = protoclust(dist(x))
  sort_heights = sort(dendro$height)

  if ((2 * test_k) < n) {
    select_heights = sort_heights[seq(1, 2 * test_k, by = 2)]
  } else {
    select_heights = sort_heights[1:test_k]
  }
  test_index = which(dendro$height %in% select_heights)

  split = list(
    train = dendro$order[-test_index],
    test = dendro$order[test_index]
  )
  split
}
