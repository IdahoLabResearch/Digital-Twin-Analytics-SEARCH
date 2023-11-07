#' Multiple imputation of missing values
#'
#' Multiple imputation of missing values.
#'
#' Copyright 2023 Battelle Energy Alliance ALL RIGHTS RESERVED
#'
#' @param x (matrix) A matrix of values in which there are NA values.
#' @param seedval (integer) The seed for imputation.
#' @return fit (model specific object) The model fit.
#'
#' @import mice
#' @export exploreImpute
exploreImpute = function(x, seedval = NULL) {
  if (is.null(setseed)) {
    setseed = 12345
  }
  imp = mice(x, m = 5, maxit = 50, printFlag = FALSE, seed = setseed)
  x = complete(imp)

  x
}
