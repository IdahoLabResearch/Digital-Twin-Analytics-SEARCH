#' Reduce the via column-wise interpolation
#'
#' Reduce the via column-wise interpolation
#'
#' Copyright 2023 Battelle Energy Alliance ALL RIGHTS RESERVED
#'
#' @param x (matrix) The matrix that will be reduced.
#' @param n (integer) The desired number of rows.
#' @return x_reduced (matrix) The dimension reduced matrix.
#'
#' @importFrom stats approxfun
#' @export reduceINTERP
reduceINTERP = function(x, n){
  p = ncol(x)
  x_reduced = matrix(rep(0, n * p), n, p)
  tmp_domain = 1:nrow(x)
  new_domain = 1:n
  for (i in 1:p) {
    tmp_fun = approxfun(tmp_domain, x[, i])
    x_reduced[, i] = tmp_fun(new_domain)
  }

  x_reduced
}
