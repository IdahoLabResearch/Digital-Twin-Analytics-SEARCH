#' Reduce via the Singular Value Decomposition
#'
#' Reduce via the Singular Value Decomposition
#'
#' Copyright 2023 Battelle Energy Alliance ALL RIGHTS RESERVED
#'
#' @param x (matrix) The matrix that will be reduced.
#' @param rank (interger | NULL) The estimated rank of the matrix. Will be
#' determined using Donoho's threshold if left NULL.
#' @param scaledata (boolean) Indicator to scale the data prior to the SVD.
#' @return x_reduced (matrix) The dimension reduced matrix.
#'
#' @export reduceSVD

reduceSVD = function(x, rank = NULL, scaledata = FALSE) {
  # Mean center and standardize the matrix such that PCA and SVD are covered.
  if (scaledata) {
    x = scale(x)
  }

  # Perform the SVD of the matrix x.
  fit = svd(x)

  # If there is no rank given, then the rank will be determined as an
  # approximate thresholded value.
  if (is.null(rank)) {
    if (ncol(x) > length(fit$d)) {
      rank = svdThreshold(fit$d, ncol(x))
    } else {
      rank = ncol(x)
    }
  }

  # Return the first 1 through rank column space eigenvectors.
  x_reduced = fit$u[, 1:rank] %*% diag(fit$d[1:rank])
  attr(x_reduced, "proj") = fit$v[, 1:rank]
  attr(x_reduced, "eigenvalues") = fit$d

  x_reduced
}
