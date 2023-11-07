#' regGLMNET
#'
#' Regression via Generalized Linear Models and Elastic Net.
#'
#' This is an extension of a linear model through the combination of a
#' generalized linear model and penalization of the regression coefficients.
#' Note that an alpha value of 0 indicates ridge regression while an alpha value
#' of 1 indicates lasso regression.
#'
#' Copyright 2023 Battelle Energy Alliance ALL RIGHTS RESERVED
#'
#' @inheritParams assess
#' @param lambda (numeric vector) The weight of penalization on the regression
#' coefficients. Can be NULL where lambda is determined through theoretical
#' estimates or be provided as a vector of values to optimize over.
#' @param alpha (numeric between 0 and 1) The alpha parameter controls the
#' type of penalization. A value of 0 indicates ridge regression (only dealing
#' with multicollinearity), a value of 1 indicates lasso regression (only
#' providing variable selection), and a value of 0.5 indicates elastic net
#' (multicollinearity and variable selection)."
#' @param family (string) The distribution associated with the errors, e.g.,
#' gaussian for a linear model or binomial for logistic.
#' @return fit (model specific object) The model fit.
#'
#'
#' @import glmnet
#' @export regENET
regENET = function(x, lambda = NULL, alpha = 0.5,
                   family = c("gaussian", "binomial", "poisson", "mguassian")) {
  y = x$y
  x = x$x

  startTime = proc.time()
  ##############################################################################
  ##################### Insert algorithm specific code here ####################
  ##############################################################################
  family = match.arg(family)
  x = as.matrix(x)
  fit = try(cv.glmnet(x, y, alpha = alpha, lambda = lambda, family = family))
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
  class(fit) = "regGLMNET"
  attr(fit, "hyperparamters") = list(family = family, lambda = lambda,
                                     alpha = alpha)

  fit
}


#' @rdname holistic_model
#' @export
holistic_model.regGLMNET = function(object, ...) {
  out = list(
    model = "regGLMNET",
    tag = "Regression via Generalized Linear Models and Elastic Net",
    descrip = "This is an extension of a linear model through the combination of a generalized linear model and penalization of the regression coefficients. Note that an alpha value of 0 indicates ridge regression while an alpha value of 1 indicates lasso regression. If the model does well when alpha is equal to 0.5, then there could exist high collinearity between the original variables and removal of some of the variables increases the ability to predict the given response. This model is specifically useful in determining which information is required for prediction of a given response. As such, it can be used as a diagnostic tool in determining if a process is out of spec based on the change in a select few variables.",
    pros = c("Non-Gaussian errors", "Extension of linear models",
             "Deals with multicollinearity", "Provides variable selection"),
    cons = c("Must choose family of errors appropriately",
             "Requires tuning of the hyperparameter lambda"),
    args = list(
      family = "The distribution associated with the errors, e.g., gaussian for a linear model or binomial for logistic.",
      lambda = "The weight of penalization on the regression coefficients. Can be NULL where lambda is determined through theoretical estimates or be provided as a vector of values to optimize over.",
      alpha = "The alpha parameter controls the type of penalization. A value of 0 indicates ridge regression (only dealing with multicollinearity), a value of 1 indicates lasso regression (only providing variable selection), and a value of 0.5 indicates elastic net (multicollinearity and variable selection)."
    ),
    assumptions = c("no missing values")
  )

  out
}
