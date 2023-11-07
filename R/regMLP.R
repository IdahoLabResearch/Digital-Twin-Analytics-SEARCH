#' MLP sklearn
#'
#' Regression via Multi-Layer Perceptron
#'
#' The Multi-Layer Perceptron (MLP) is a feed forward artificial neural network.
#' This function utilizes the scikit-learn MLPRegressor function.
#'
#' Copyright 2023 Battelle Energy Alliance ALL RIGHTS RESERVED
#'
#' @inheritParams assess
#' @param activation (string) The activation function between nodes.
#' @return fit (model specific object) The model fit.
#'
#' @import reticulate
#' @export regMLP

regMLP = function(x, activation = c("relu", "identity", "logistic", "tanh")) {
  y = x$y
  x = x$x

  startTime = proc.time()
  ##############################################################################
  ##################### Insert algorithm specific code here ####################
  ##############################################################################
  activation = match.arg(activation)
  use_condaenv("search")
  nn = import("sklearn.neural_network")
  import("pandas")
  import("numpy")

  py_x = r_to_py(x)
  py_y = r_to_py(y)
  fit = try(nn$MLPRegressor(activation = activation))
  ##############################################################################
  ##################### End algorithm specific code ############################
  ##############################################################################
  attr(fit, "time") = as.numeric((proc.time() - startTime)[3])

  if ("try-error" %in% class(fit)) {
    fit = list(fit = "NA")
    class(fit) = "try_error"
    return(fit)
  }

  fit = fit$fit(py_x, py_y)
  class(fit) = c("regMLP", class(fit))
  attr(fit, "hyperparamters") = list(activation = activation)

  fit
}


#' @rdname holistic_model
#' @export
holistic_model.regMLP = function(object, ...) {
  out = list(
    model = "regMLP",
    tag = "Regression via Multi-Layer Perceptron",
    descrip = "The Multi-Layer Perceptron (MLP) is a feed forward artificial neural network. This function utilizes the scikit-learn MLPRegressor function. Depending on the number of nodes, layers, and activation function, the MLP algorithm can due well when there exists non-linearities with respect to the response and the set of variables. However, this model does not help with inference of why the relationship between the response and the variables exist. This model should be chosen for prediction rather than explination. Additionally, neural networks tend to require a vast amount of data for accurate prediction.",
    pros = c("Nonlinear relationships"),
    cons = c("May require a large amount of data", "Difficult to interpret"),
    args = list(
      activation = "The activation function between nodes."
    ),
    assumptions = c("numeric independent variables", "no missing values")
  )

  out
}

