#' Classification test data
#'
#' Generate the classification test data from chem_data.
#'
#' Copyright 2023 Battelle Energy Alliance ALL RIGHTS RESERVED
#'
#' @return data (list) The model independent variables and response.
#'
#'
#' @export test_classification_data_no_test

test_classification_data_no_test = function() {
  data = search::chem_data
  data = data[data$class %in% c(1,2), ]
  y = data$class
  y[y == 2] = 0
  x = data[, -which(names(data) == "class")]

  list(x = x, y = y)
}
