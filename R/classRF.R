#' Classification using Random Forest
#'
#' Classification using Random Forest
#'
#' Copyright 2023 Battelle Energy Alliance ALL RIGHTS RESERVED
#'
#' @inheritParams assess
#' @return fit (model specific object) The model fit.
#'
#' @import randomForest
#' @export classRF
classRF = function(x) {
  y = x$y
  x = x$x

  startTime = proc.time()
  ##############################################################################
  ##################### Insert algorithm specific code here ####################
  ##############################################################################
  x$y = as.factor(y)
  fit =
  fit = try(randomForest(y ~ ., data = x, importance = TRUE, proximity = TRUE))
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
  class(fit) = "classRF"

  fit
}
