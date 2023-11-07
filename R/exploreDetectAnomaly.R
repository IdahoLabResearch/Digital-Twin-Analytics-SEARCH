#' Detect time series outliers
#'
#' Detect time series outliers. Each column is assumed to be a time series.
#' Copyright 2023 Battelle Energy Alliance ALL RIGHTS RESERVED
#'
#' @param x (vector) A vector of values in which there are suspected outliers.
#' @return A vector of indicies of suspected outliers.
#'
#' @importFrom forecast tsoutliers
#' @importFrom kernlab ksvm
#' @export exploreDetectAnomaly
exploreDetectAnomaly = function(x) {
  ts_index = tsoutliers(x)$index
  svm_fit = ksvm(x, type="one-svc", kernel="vanilladot")
  svm_predict = kernlab::predict(svm_fit)
  svm_index = which(svm_predict[,1] == TRUE)
  norm_index = which(abs(x) >
                          (mean(x, na.rm = TRUE) + 3 * sd(x, na.rm = TRUE)))

  list(ts = ts_index, svm = svm_index, norm = norm_index)
}
