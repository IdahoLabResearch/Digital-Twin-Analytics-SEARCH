#' Regression test data
#'
#' Generate the regression test data from chem_data.
#'
#' Copyright 2023 Battelle Energy Alliance ALL RIGHTS RESERVED
#'
#' @return data (list) The model independent variables and response.
#'
#'
#' @export test_regression_data_no_test

test_regression_data_no_test = function() {
  data = search::chem_data
  # remove the class variable
  data = data[, -1]
  y = data[, 1]
  x = data[, -1]

  list(x = x, y = y)
}
