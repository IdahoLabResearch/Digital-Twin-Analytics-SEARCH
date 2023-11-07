#' Confusion values
#'
#' The false negative, false positive, true negative, and true positive
#' values for each class.
#'
#' Copyright 2023 Battelle Energy Alliance ALL RIGHTS RESERVED
#'
#' @inheritParams confusion
#' @return confusion_values (list) The false negative, false positive,
#' true negative, and true positive values for each class.
#'
#' @export confusionValues
confusionValues = function(observed, predicted) {
  unique_class = as.character(sort(unique(observed)))
  observed = as.character(observed)
  predicted = as.character(predicted)
  n = length(observed)

  result = list()
  for (i in unique_class) {
    obs = rep(0, n)
    pred = rep(0, n)
    obs[observed == i] = 1
    pred[predicted == i] = 1
    obs_diff = obs - pred

    iter_result = list(
      fn = sum(obs_diff == 1),
      fp = sum(obs_diff == -1),
      tp = sum((obs == 1) & (pred == 1)),
      tn = sum((obs == 0) & (pred == 0))
    )
    result[[i]] = iter_result
  }

  result
}
