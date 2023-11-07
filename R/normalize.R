#' Normalize
#'
#' Subtract the mean and divide by the standard deviation. If given [params],
#' then will apply a given mean and standard deviation to the data set.
#' Otherwise, will calculate the mean and standard deviation. If [norm_params]
#' is not NULL, will reverse the normalization process.
#'
#' Copyright 2023 Battelle Energy Alliance ALL RIGHTS RESERVED
#'
#' @param x (matrix, data frame, or scalar vector) The numeric values to be
#' normalized.
#' @param params (list) The associated values for the normalization process.
#' @param norm_params (list) The associated values required for reversing the
#' normalization process.
#' @return result (list) The normalized values and normalization parameters.
#'
#' @importFrom stats sd
#' @export normalize

normalize = function(x, params = NULL, norm_params = NULL) {
  is_vec = FALSE
  if (is.null(ncol(x))) {
    x = matrix(x, ncol = 1)
    is_vec = TRUE
  }

  if (is.null(norm_params)) {
    if (is.null(params)) {
      x_mean = apply(x, 2, mean)
      x_sd = apply(x, 2, sd)
    } else {
      x_mean = params$mean
      x_sd = params$sd
      if (ncol(x) != length(x_mean)) {
        x_mean = rep(x_mean[1], ncol(x))
        x_sd = rep(x_sd[1], ncol(x))
      }
    }
    x = sweep(x, 2, x_mean)
    x = sweep(x, 2, x_sd, "/")
    norm_params = list(mean = x_mean, sd = x_sd)
  } else {
    x = sweep(x, 2, norm_params$sd, "*")
    x = sweep(x, 2, norm_params$mean, "+")
    norm_params = NULL
  }

  if (is_vec) {
    x = as.numeric(x)
  }

  list(x = x, norm_params = norm_params)
}
