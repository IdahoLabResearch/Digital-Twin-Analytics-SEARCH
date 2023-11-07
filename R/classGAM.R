#' Classification using Generalized Additive Models
#'
#' Classification using Generalized Additive Models
#'
#' Copyright 2023 Battelle Energy Alliance ALL RIGHTS RESERVED
#'
#' @inheritParams regLM
#' @return fit (model specific object) The model fit.
#'
#' @import mgcv
#' @importFrom stats as.formula
#' @export classGAM
classGAM = function(x) {
  y = x$y
  x = x$x

  startTime = proc.time()
  ##############################################################################
  ##################### Insert algorithm specific code here ####################
  ##############################################################################
  x_names = colnames(x)
  max_k = 10
  x_preds = paste0(" + s(", x_names, ", k = ", max_k, ")", collapse = "")
  x_preds = substr(x_preds, 4, nchar(x_preds))
  tmp_formula = as.formula(paste0("y ~ ", x_preds))
  x$y = y
  fit = try(gam(tmp_formula, data = x, family = "binomial"))
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
  class(fit) = "classGAM"

  fit
}
