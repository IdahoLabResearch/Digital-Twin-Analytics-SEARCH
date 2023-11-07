#' Reduce via the Dynamic Mode Decomposition
#'
#' Reduce via the Dynamic Mode Decomposition
#'
#' Copyright 2023 Battelle Energy Alliance ALL RIGHTS RESERVED
#'
#' @param x (matrix) The matrix that will be reduced.
#' @param rank (interger | NULL) The estimated rank of the matrix. Will be
#' determined using Donoho's threshold if left NULL.
#' @return x_reduced (matrix) The dimension reduced matrix.
#'
#' @export reduceDMD
reduceDMD = function(x, rank = NULL) {
  # Mean center and standardize the matrix such that PCA and SVD are covered.
  x = scale(x)

  x1 = x[, 1:(ncol(x) - 1)]
  x2 = x[, 2:ncol(x)]

  # Perform the SVD of the matrix x1.
  fit  = svd(x1)

  # If there is no rank given, then the rank will be determined as an
  # approximate thresholded value.
  if (is.null(rank)) {
    rank = svdThreshold(fit$d, ncol(x1))
  }

  if (rank > ncol(x1)) {
    rank = ncol(x1)
  }

  U = matrix(fit$u[, 1:rank], ncol = rank)
  V = matrix(fit$v[, 1:rank], ncol = rank)
  d = fit$d[1:rank]

  # Ensuring matrix dimensions with rank 1 estimates
  if ( rank == 1 ) {
    d_inv = matrix(1 / d, ncol = 1)
  } else {
    d_inv = solve(diag(d))
  }

  atilde = t(U) %*% x2 %*% V %*% d_inv
  eigen_decomp = eigen(atilde)
  modes = x2 %*% V %*% d_inv %*% eigen_decomp$vectors

  x_reduced = matrix((Re(modes)), ncol = rank)

  x_reduced
}
