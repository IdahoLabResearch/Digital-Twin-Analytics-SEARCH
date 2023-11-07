# Copyright 2023 Battelle Energy Alliance ALL RIGHTS RESERVED
#' @rdname assess
#' @export
assess.default = function(x, ...) {
  fit = mean(x$y)
  class(fit) = "mean"
  fit
}

#' Create a model fit based on the data.
#'
#' Copyright 2023 Battelle Energy Alliance ALL RIGHTS RESERVED
#'
#' @param x (list) A list containing [x], the set of independent variables as a
#' data frame and [y], the response as a vector.
#' @param ... Additional arguments
#' @return model (object) The model of a data
#'
#' @export
#' @export assess
assess = function(x, ...)
  UseMethod("assess")
