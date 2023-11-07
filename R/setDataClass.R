#' Set Data Class
#'
#' Verify the dimensions of the data as well as determines the appropriate
#' class for the data.
#'
#' Copyright 2023 Battelle Energy Alliance ALL RIGHTS RESERVED
#'
#' @param x (list) A list containing [x], the set of independent variables and
#' [y], the response.
#' @return result (list) The training and test as a data frame.
#'
#' @export setDataClass
setDataClass = function(x) {
  default_name = "Var1"
  # Transform train into a matrix if given a vector.
  if (is.null(ncol(x$x))) {
    x$x = matrix(x$x, ncol = 1)
    colnames(x$x) = default_name
  }

  dim_type = "low"
  if (ncol(x$x) > nrow(x$x) | ncol(x$x) >= 50) {
    dim_type = "high"
  }

  # Need binary encoding of categorical variables
  # Need to include multinomial
  unique_y = unique(x$y)

  model_type = "Reg"
  if (length(unique_y) == 2) {
    model_type = "Class"
  }


  # # Remove any additional columns from test that are not contained in train.
  # test = test[, colnames(test) %in% colnames(train)]
  # if (is.null(ncol(test))) {
  #   test = matrix(test, ncol = 1)
  #   colnames(test) = default_name
  # }

  # # Check for any mismatch of variable names.
  # if (ncol(train) > ncol(test)) {
  #   missing_names = colnames(train)[!(colnames(train) %in% colnames(test))]
  #   stop(paste0("The test data set is missing the following variables: ",
  #               missing_names))
  # }

  x$x = as.data.frame(x$x)
  class(x) = paste0(dim_type, model_type)
  x
}
