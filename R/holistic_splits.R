#' Holistic model assessment with multiple splits
#'
#' Wrapper for comparing all models over each split.
#'
#' Copyright 2023 Battelle Energy Alliance ALL RIGHTS RESERVED
#'
#' @param x (data frame or matrix) A set of independent variables.
#' @param y (vector) The response.
#' @param num_splits (integer) The total number of iterations of splits.
#' @param n_core (integer) The total number of cores to use.
#' @return metrics_list (list of metrics per model) The metrics associated with
#' each metric and model and split.
#'
#' @import parallel
#' @export holistic_splits

holistic_splits = function(x, y, num_splits = 5, n_core = NULL) {
  if (is.null(n_core)) {
    n_core = detectCores() - 1
  }

  metrics_list = list()
  if (n_core == 1) {
    for (i in num_splits) {
      metrics_list[[i]] = holistic_models(x, y)
    }
  } else {
    x_list = replicate(num_splits, x, simplify=FALSE)
    cl = makeCluster(n_core)
    metrics_list = lapply(x_list, holistic_models, y = y)
    stopCluster(cl)
  }

  metrics_list
}
