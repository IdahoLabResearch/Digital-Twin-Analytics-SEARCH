# Copyright 2023 Battelle Energy Alliance ALL RIGHTS RESERVED
#' @rdname holistic_model
#' @export
holistic_model.default = function(object, ...) {
  "No specific information for this model."
}

#' Create holistic documentation for a given model.
#'
#' Copyright 2023 Battelle Energy Alliance ALL RIGHTS RESERVED
#'
#' @param object (object) A fit object.
#' @param ... Additional arguments
#' @return doc (string) The documentation for a given model.
#'
#' @export
#' @export holistic_model
holistic_model = function(object, ...)
  UseMethod("holistic_model")
