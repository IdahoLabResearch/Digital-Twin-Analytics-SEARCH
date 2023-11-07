#' Holistic model assessment
#'
#' Wrapper for comparing all models.
#'
#' Copyright 2023 Battelle Energy Alliance ALL RIGHTS RESERVED
#'
#' @param x (data frame or matrix) A set of independent variables.
#' @param y (vector) The response.
#' @return metrics (list of metrics per model) The metrics associated with each
#' metric and model.
#'
#' @importFrom stats predict
#' @export holistic_models

holistic_models = function(x, y) {
  # Run the splits (explore)
  split = split_rand(x)
  x_train = x[split$train, ]
  x_test = x[split$test, ]
  y_train = y[split$train]
  y_test = y[split$test]
  y_observed = list(
    train = y_train,
    test = y_test
  )

  # Normalize the data (explore)
  x_norm = normalize(x_train)
  x_params = x_norm$norm_params
  x_norm = list(
    train = x_norm$x,
    test = normalize(x_test, params = x_params)$x
  )
  if (is.character(y_train)) {
    y_norm = y_train
    y_params = NULL
  } else {
    y_norm = normalize(y_train)
    y_params = y_norm$norm_params
    y_norm = y_norm$x
  }

  # Identify models that are available for a given x and y (assess)
  data = list(x = x_norm$train, y = y_norm)
  data_test = list(x = x_norm$test, y = y_test)
  data = setDataClass(data)
  fits = assess(data)

  result = list()
  model_resids = list()
  fitted_vals = list()
  for (i in names(fits)) {
    # Predict training data
    pred_vals = predict(fits[[i]], data)
    pred_vals$observed = y_train
    if (!is.null(y_params)) {
      pred_vals$predicted = normalize(pred_vals$predicted,
                                      norm_params = y_params)$x
    }
    train_metrics = confirm(pred_vals)
    fitted_vals[[i]] = pred_vals$predicted
    model_resids[[i]] = pred_vals$observed - pred_vals$predicted

    # Predict test data
    pred_vals = predict(fits[[i]], data_test)
    if (!is.null(y_params)) {
      pred_vals$predicted = normalize(pred_vals$predicted,
                                      norm_params = y_params)$x
    }
    test_metrics = confirm(pred_vals)

    result[[i]] = list(train = train_metrics, test = test_metrics)
  }

  list(result = result, fits = fits, resids = model_resids,
       fitted = fitted_vals)
}

