#' regLM
#'
#' Regression via a Linear Model
#'
#' Application of a Ordinary Least Squares model.
#'
#' Copyright 2023 Battelle Energy Alliance ALL RIGHTS RESERVED
#'
#' @inheritParams assess
#' @return fit (model specific object) The model fit.
#'
#' @importFrom stats lm
#' @export regLM
regLM = function(x) {
  y = x$y
  x = x$x

  startTime = proc.time()
  ##############################################################################
  ##################### Insert algorithm specific code here ####################
  ##############################################################################
  x = as.data.frame(x)
  x$y = y
  fit = try(lm(y ~ ., data = x))
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
  class(fit) = "regLM"

  fit
}


#' @rdname holistic_model
#' @export
holistic_model.regLM = function(object, ...) {
  out = list(
    model = "regLM",
    tag = "Regression via a Linear Model",
    descrip = "Application of a Ordinary Least Squares model. This is the simplest regression model, but has specific data assumptions to ensure that it works properly. More specifically, that there are no outliers, the response relationship is approximately linear, there exists no multi-collinearity between variables, the residuals are approximately Gaussian, the residuals are not auto-correlated with themselves, and the residuals have equal variance. In application, many of these assumptions may be violated and hence the reason for the use of more complex models that handle specific assumptions.",
    pros = c("Simple"),
    cons = c("low-dimensional"),
    args = list(none = "none"),
    assumptions = c("low-dimensional", "no missing values")
  )

  out
}

