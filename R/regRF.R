#' regRF
#'
#' Regression via Random Forest
#'
#' The Random Forest (RF) technique is an ensemble learning method by creating
#' multiple decision trees.
#'
#' Copyright 2023 Battelle Energy Alliance ALL RIGHTS RESERVED
#'
#' @inheritParams assess
#' @param ntree (integer) The total number of trees to use in building the
#' random forest.
#' @return fit (model specific object) The model fit.
#'
#' @import randomForest
#' @export regRF
regRF = function(x, ntree = 500) {
  y = x$y
  x = x$x

  startTime = proc.time()
  ##############################################################################
  ##################### Insert algorithm specific code here ####################
  ##############################################################################
  x = as.matrix(x)
  fit = try(randomForest(x, y, ntree = ntree))
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
  class(fit) = "regRF"
  attr(fit, "hyperparamters") = list(ntree = ntree)

  fit
}

#' @rdname holistic_model
#' @export
holistic_model.regRF = function(object, ...) {
  out = list(
    model = "regRF",
    tag = "Regression via Random Forest",
    descrip = "The Random Forest (RF) technique is an ensemble learning method by creating multiple decision trees.",
    pros = c("Robust to outliers", "Nonlinear relationships"),
    cons = c("Slow in optimization when using large data sets", "May overfit training data"),
    args = list(
      ntree = "The total number of trees to use in building the random forest."
    ),
    assumptions = c("no missing values")
  )

  out
}


