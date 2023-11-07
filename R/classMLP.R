#' MLP sklearn classifier
#'
#' Wrapper for scikit-learns MLP classifier.
#'
#' Copyright 2023 Battelle Energy Alliance ALL RIGHTS RESERVED
#'
#' @inheritParams assess
#' @return fit (model specific object) The model fit.
#'
#' @import reticulate
#' @export classMLP
classMLP = function(x) {
  y = x$y
  x = x$x

  startTime = proc.time()
  ##############################################################################
  ##################### Insert algorithm specific code here ####################
  ##############################################################################
  nn = import("sklearn.neural_network")
  import("pandas")
  import("numpy")

  py_x = r_to_py(x)
  py_y = r_to_py(y)
  fit = try(nn$MLPClassifier(activation = 'logistic',
                            validation_fraction = 0.95,
                            random_state = as.integer(1),
                            beta_1 = 0.95,
                            beta_2 = 0.95,
                            max_iter = as.integer(1000),
                            early_stopping = FALSE))
  ##############################################################################
  ##################### End algorithm specific code ############################
  ##############################################################################
  attr(fit, "time") = as.numeric((proc.time() - startTime)[3])

  if ("try-error" %in% class(fit)) {
    fit = list(fit = "NA")
    class(fit) = "try_error"
    return(fit)
  }

  fit = fit$fit(py_x, py_y)
  class(fit) = c("classMLP", class(fit))
  fit
}
