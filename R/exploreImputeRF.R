#' Multiple imputation of missing values
#'
#' Multiple imputation of missing values.
#'
#' Copyright 2023 Battelle Energy Alliance ALL RIGHTS RESERVED
#'
#' @param x (matrix) A matrix of values in which there are NA values.
#' @param y (vector) A response to predict based on the independent variables.
#' @param ntree (integer) The number of trees in the forest.
#' @return fit (model specific object) The model fit.
#'
#' @import randomForest
#' @export exploreImputeRF
exploreImputeRF = function(x, y, ntree = 500) {
  imp = rfImpute(x, y, ntree = ntree)
  imp
}
