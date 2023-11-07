#' regSVM
#'
#' Regression via Support Vector Machine
#'
#' The Support-Vector Machine (SVM) technique is based on optimizing a specific
#' hyperplane (or sets of hyperplanes) between the data set variables.
#'
#' Copyright 2023 Battelle Energy Alliance ALL RIGHTS RESERVED
#'
#' @inheritParams assess
#' @param kernel (string) The kernel associated with the nonlinear behaviors of
#' the data set.
#' @param gamma (numeric scalar) A tuning parameter based on the kernel. The
#' linear kernel does not require a value of gamma.
#' @return fit (model specific object) The model fit.
#'
#' @import e1071
#' @export regSVM
regSVM = function(x, kernel = c("linear", "polynomial", "radial", "sigmoid"),
                  gamma = NULL) {
  y = x$y
  x = x$x

  startTime = proc.time()
  ##############################################################################
  ##################### Insert algorithm specific code here ####################
  ##############################################################################
  if (is.null(gamma)) {
    gamma = 1 / ncol(x)
  }
  x = as.matrix(x)
  fit = try(svm(x, y, gamma = gamma))
  ##############################################################################
  ##################### End algorithm specific code ############################
  ##############################################################################
  attr(fit, "time") = as.numeric((proc.time() - startTime)[3])

  if ("try-error" %in% class(fit)) {
    fit = list(fit = "NA")
    class(fit) = "try_error"
    return(fit)
  }

  fit$orig_class = class(fit)
  class(fit) = "regSVM"
  attr(fit, "hyperparamters") = list(kernel = kernel, gamma = gamma)

  fit
}

#' @rdname holistic_model
#' @export
holistic_model.regSVM = function(object, ...) {
  out = list(
    model = "regSVM",
    tag = "Regression via Support Vector Machine",
    descrip = "The Support-Vector Machine (SVM) technique is based on optimizing a specific hyperplane (or sets of hyperplanes) between the data set variables.",
    pros = c("Nonlinear relationships", "Flexible with different kernels"),
    cons = c("Slow in optimization when using large data sets", "Lack of interpretability"),
    args = list(
      kernel = "The kernel associated with the nonlinear behaviors of the data set.",
      gamma = "A tuning parameter based on the kernel. The linear kernel does not require a value of gamma."
    ),
    assumptions = c("no missing values")
  )

  out
}

