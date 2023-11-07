#' regGLM
#'
#' Regression via Generalized Linear Models
#'
#' The Generalized Linear Model (GLM) is an extension of a linear model where the
#' independent variables and the dependent variable share a potentially
#' non-Gaussian relationship.
#'
#' Copyright 2023 Battelle Energy Alliance ALL RIGHTS RESERVED
#'
#' @inheritParams assess
#' @param family (string) The distribution associated with the errors, e.g.,
#' "gaussian" for a linear model or "binomial" for logistic.
#' @return fit (model specific object) The model fit.
#'
#' @importFrom stats glm
#' @export regGLM

regGLM = function(x, family = c("gaussian", "binomial", "quasibinomial",
                                "poisson", "quasipoisson")) {
  y = x$y
  x = x$x

  startTime = proc.time()
  ##############################################################################
  ##################### Insert algorithm specific code here ####################
  ##############################################################################
  family = match.arg(family)
  x = as.data.frame(x)
  fit = try(glm(y ~ ., data = x, family = family))
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
  attr(fit, "hyperparamters") = list(family = family)

  fit
}


#' @rdname holistic_model
#' @export
holistic_model.regGLM = function(object,...) {
  out = list(
    model = "regGLM",
    tag = "Regression via Generalized Linear Models",
    descrip = "The generalized linear model is an extension of a linear model where the independent variables and the dependent variable share a potentially non-gaussian relationship. For example, what if the variables describe a rate of change for the response or includes count information. Then the response should be modeled with an exponential change (using a Poisson distribution). This can also be extended to a binomial distribution where the change in the response is a sigmoidal relationship (as can be seen when performing classification). When the GLM model performs well, then the user should examine if there exists a non-linear relationship between the response and the variables. The choice of distribution can be used to infer or explain some of the physical quantities of the process.",
    pros = c("Non-Gaussian errors", "Extension of linear models"),
    cons = c("Must choose family of errors appropriately", "low-dimensional"),
    args = list(
      family = "The distribution associated with the errors, e.g., gaussian for a linear model or binomial for logistic."
    ),
    assumptions = c("low-dimensional", "no missing values")
  )

  out
}

