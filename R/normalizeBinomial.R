#' Normalize Binomial
#'
#' Transform the variable to be between zero and one. If [norm_params] is NULL, then
#' will normalize the data. Otherwise, will reverse the normalization process.
#'
#' Copyright 2023 Battelle Energy Alliance ALL RIGHTS RESERVED
#'
#' @inheritParams normalize
#' @return result (list) The normalized values and normalization parameters.
#'
#' @export normalizeBinomial

normalizeBinomial = function(x, params = NULL, norm_params = NULL) {
  is_vec = FALSE
  if (is.null(ncol(x))) {
    x = matrix(x, ncol = 1)
    is_vec = TRUE
  }

  # If norm_params is null, then proceed with normalization. Otherwise, will
  #reverse the normalization.
  if (is.null(norm_params)) {
    if (is.null(params)) {
      x_min = apply(x, 2, min, na.rm = T) - 1e-5
      x_max = apply(x, 2, max, na.rm = T)
    } else {
      x_min = params$min
      x_max = params$max
      if (ncol(x) != length(x_min)) {
        x_mean = rep(x_min[1], ncol(x))
        x_max = rep(x_max[1], ncol(x))
      }
    }
    x = sweep(x, 2, x_min)
    x = sweep(x, 2, (x_max - x_min), "/")
    norm_params = list(min = x_min, max = x_max)
  } else {
    x = sweep(x, 2, (norm_params$max - norm_params$min), "*")
    x = sweep(x, 2, norm_params$min, "+")
    norm_params = NULL
  }

  if (is_vec) {
    x = as.numeric(x)
  }

  list(x = x, norm_params = norm_params)
}
