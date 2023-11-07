#' Linear regression
#'
#' Wrapper for linear Regression
#'
#' Copyright 2023 Battelle Energy Alliance ALL RIGHTS RESERVED
#'
#' @inheritParams assess
#' @return fit (model specific object) The model fit.
#'
#' @importFrom stats glm
#' @export classLOGISTIC
classLOGISTIC = function(x) {
  y = x$y
  x = x$x

  startTime = proc.time()
  ##############################################################################
  ##################### Insert algorithm specific code here ####################
  ##############################################################################
  x = as.data.frame(x)
  x$y = y
  fit = try(glm(y ~ ., data = x, family = "binomial"))
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
  class(fit) = "regGLM"

  fit
}
