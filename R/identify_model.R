#' Identify models
#'
#' Identify a list of models and metrics based on the data set.
#'
#' Copyright 2023 Battelle Energy Alliance ALL RIGHTS RESERVED
#'
#' @param x (data frame or matrix) A set of independent variables.
#' @param y (vector) The response.
#' @return models (list of string vectors) The models and metrics to run.
#'
#' @export identify_model

identify_model = function(x, y) {

  # "reg_binomial", "reg_poisson", need an additional filter for between 0 and 1
  # also for non-negative values
  reg_low_dim = c("regLM",  "regGAM")
  reg_high_dim = c("regENET",  "regLASSO", "regRIDGE", "regSVM", "regRF",
                   "regMLP")

  # class_low_dim =

  # Verification of current data
  is_regression = TRUE
  if (is.character(y)) {
    is_regression = FALSE
  }
  is_high_dim = is.high_dimensional(x)

  if (is_regression) {
    model_names = reg_high_dim
    metric_names = c("r2", "rmse", "mape", "smape")
    if(!is_high_dim) {
      model_names = c(model_names, reg_low_dim)
    }
  }

  list(models = model_names, metrics = metric_names)
}
