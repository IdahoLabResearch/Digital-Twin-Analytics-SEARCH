#' Reduce the via STL
#'
#' Reduce the via STL
#'
#' Copyright 2023 Battelle Energy Alliance ALL RIGHTS RESERVED
#'
#' @param x (matrix) The matrix that will be reduced.
#' @param n (integer) The desired number of rows.
#' @param nknots (integer) The number of knots to use for the spline fit.
#' @return x_reduced (matrix) The dimension reduced matrix.
#'
#' @importFrom mgcv gam
#' @export reduceGAM
reduceGAM = function(x, n, nknots = NULL){
  p = ncol(x)
  x_reduced = matrix(rep(0, n * p), n, p)
  tmp_domain = 1:nrow(x)
  new_domain = 1:n
  if (is.null(nknots)) {
    nknots = ceiling(nrow(x) * 0.02)
  }

  for (i in 1:p) {
    # setup some data frame for the gam model
    df = data.frame(y = x[, i], x = tmp_domain)
    new_df = data.frame(y = rep(0, n), x = new_domain)
    fit = gam(y ~ s(x, k = nknots), method = "REML", data = df)
    x_reduced[, i] = predict(fit, newdata = new_df)
  }

  x_reduced
}
