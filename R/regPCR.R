#' regPCR
#'
#' Regression via a Principal Component Regression
#'
#' Application of dimension reduction and regression.
#'
#' Copyright 2023 Battelle Energy Alliance ALL RIGHTS RESERVED
#'
#' @inheritParams assess
#' @return fit (model specific object) The model fit.
#'
#' @importFrom stats lm
#' @export regPCR
regPCR = function(x) {
  y = x$y
  x = x$x

  startTime = proc.time()
  ##############################################################################
  ##################### Insert algorithm specific code here ####################
  ##############################################################################

  newx = reduceSVD(x)
  if (ncol(newx) > nrow(newx)) {
    newx = reduceSVD(x, rank = nrow(x) - 1)
  }
  eigenvalues = attr(newx, "eigenvalues")
  proj = attr(newx, "proj")

  newx = as.data.frame(newx)
  newx$y = y
  fit = try(lm(y ~ ., data = newx))
  attr(fit, "proj") = proj
  attr(fit, "eigenvalues") = eigenvalues
  attr(fit, "scores") = newx[, -which(names(newx) == "y")]


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
  class(fit) = "regPCR"

  fit
}


#' @rdname holistic_model
#' @export
holistic_model.regPCR = function(object, ...) {
  out = list(
    model = "regPCR",
    tag = "Regression via a Principal Component Regression",
    descrip = "Application of dimension reduction and regression. The original set of data is reduced to an orthogonal linear combination of the original variables. This means that the variables are compressed in such a way that it reduces the number of rows while retaining a large percent of the information contained in the data set. As the new varibles are orthogonal and the new variables are less than the number of row, ordinary least squares can be applied for prediction of the new data. If this model does well, then there could exist a large set of repeated or correlated variables or a large amount of process noise.",
    pros = c("Reduced dimension."),
    cons = c("Variables are now a linear combination of original variables."),
    args = list(none = "none"),
    assumptions = c("no missing values")
  )

  out
}

