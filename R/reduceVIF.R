#' Reduce via Variance Inflation Factor
#'
#' Reduce via Variance Inflation Factor
#'
#' Copyright 2023 Battelle Energy Alliance ALL RIGHTS RESERVED
#'
#' @param x (matrix) The matrix that will be reduced.
#' @param threshold (numeric scalar between 0 and 1) The maximum correlation for
#' a given variable.
#' @return x_reduced (matrix) The dimension reduced matrix.
#'
#' @importFrom glmnet glmnet
#' @export reduceVIF
reduceVIF = function(x, threshold = 0.95) {
  # Can only be used on matrices with number of columns greater than 2.
  if (ncol(x) < 2) {
    return(x)
  }

  # Mean center and standardize the matrix.
  x = scale(x)

  # Need to create a separate function for this
  r2_loop = function(x) {
    # calculate the VIF per variable using ridge regression
    all_r2 = rep(0, ncol(x))
    for (i in 1:ncol(x)) {
      # Using a quick version of ridge regression to determine the r2 per
      #   variable.
      # Need to cite Park et al for the theory behind using a small value as the
      #   tuning parameter.
      dependent = x[, i]
      independent = x[, -i]

      fit = glmnet(x = independent, y = dependent, alpha = 0, lambda = 0.001)
      predicted = predict(fit, independent)
      all_r2[i] = r2(dependent, predicted)
    }
    all_r2
  }

  # Using backwards step-wise VIF removal.
  removed_idx = NULL
  max_iter = ncol(x) - 2
  iter = 0
  while (iter < max_iter) {
    tmp_r2 = r2_loop(x)

    # Grab the maximum r2 index.
    max_index = which.max(tmp_r2)
    # just in case there are multiple instances of the max.
    max_index = max_index[length(max_index)]

    if (tmp_r2[max_index] < threshold) {
      break
    } else {
      iter = iter + 1
      x = x[, -max_index]
    }
  }

  x
}
