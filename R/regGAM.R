#' regGAM
#'
#' Regression via Generalized Additive Models
#'
#' The Generalized Additive Model (GAM) is an extension of the generalized
#' linear model while assuming the independent variables can be represented
#' as smooth functions.
#'
#' Copyright 2023 Battelle Energy Alliance ALL RIGHTS RESERVED
#'
#' @inheritParams assess
#' @param family (string) The distribution associated with the errors, e.g.,
#' "gaussian" for a linear model or "binomial" for logistic.
#' @param k (integer) Number of knots associated with each variable when
#' creating a smooth function.
#' @return fit (model specific object) The model fit.
#'
#' @import mgcv
#' @export regGAM
regGAM = function(x, k = 10, family = c("gaussian",
                                        "binomial", "quasibinomial",
                                        "poisson", "quasipoisson")) {
  x_ = x
  y = x$y
  x = x$x

  startTime = proc.time()
  ##############################################################################
  ##################### Insert algorithm specific code here ####################
  ##############################################################################
  # Build the formula for GAM.
  family = match.arg(family)
  x_names = colnames(x)
  x_preds = paste0(" + s(", x_names, ", k = ", k, ")", collapse = "")
  x_preds = substr(x_preds, 4, nchar(x_preds))
  tmp_formula = as.formula(paste0("y ~ ", x_preds))

  x = as.data.frame(x)
  x$y = y

  fit = try(gam(tmp_formula, data = x, method = "REML", select = TRUE,
                family = family))
  ##############################################################################
  ##################### End algorithm specific code ############################
  ##############################################################################
  attr(fit, "time") = as.numeric((proc.time() - startTime)[3])

  if ("try-error" %in% class(fit)) {
    #fit = list(fit = "NA")
    #class(fit) = "try_error"
    #return(fit)
    k_ = k - 1 
    fit = regGAM(x_, k_)
    return(fit)
  }

  fit$orig_class = class(fit)
  class(fit) = "regGAM"
  attr(fit, "hyperparamters") = list(k = k, family = family)

  fit
}

#' @rdname holistic_model
#' @export
holistic_model.regGAM = function(object, ...) {
  out = list(
    model = "regGAM",
    tag = "Regression via Generalized Additive Models",
    descrip = "The Generalized Additive Model (GAM) is an extension of the generalized linear model while assuming the independent variables can be represented as smooth functions. This means that there exists a linear combination of transformations on the variables that explains the response. This model is particularly useful when there exists a non-linear relationship between the reponse and the set of variables. Note: if too much smoothing is applied (based on the number of knots), overfitting the training data can occur. Additionally, with an increased number of assumed knots, the training model will take more time computationaly.",
    pros = c("Nonlinear relationships", "Non-Gaussian errors",
             "Extension of linear models"),
    cons = c("Slow in optimization with more variables"),
    args = list(
      family = "The distribution associated with the errors, e.g., gaussian for
      a linear model or binomial for logistic.",
      k = "Number of knots associated with each variable when creating a smooth
      function."
    ),
    assumptions = c("low-dimensional", "numeric independent variables",
                    "no missing values")
  )

  out
}
