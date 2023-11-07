#' SVD threshold
#'
#' Determine the appropriate threshold for the singular values.
#' Donoho, David L., and Matan Gavish. "The optimal hard threshold for singular values is 4/sqrt(3)." arXiv preprint arXiv:1305.5870 4 (2013).
#'
#' Copyright 2023 Battelle Energy Alliance ALL RIGHTS RESERVED
#'
#' @param singularvalues The singular values obtained through the SVD.
#' @param p The number of columns of the matrix.
#' @return rank (integer) The approximate rank of the matrix.
#'
#' @importFrom stats median
#' @export svdThreshold
svdThreshold = function(singularvalues, p = NULL) {
  n = length(singularvalues)

  # Default lambda
  lambda = 4 / sqrt(3)
  if ( !is.null(p) ){
    beta = n / p
    # Equation 11
    lambda = sqrt(2 * (beta + 1) +
                    8 * beta / ((beta + 1) + sqrt(beta^2 + 14 * beta + 1)))
  }

  threshold = lambda * median(singularvalues)
  rank = sum(singularvalues > threshold)

  rank
}
