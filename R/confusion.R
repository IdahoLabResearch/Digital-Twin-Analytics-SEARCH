#' Confusion Matrix
#'
#' Build the confusion matrix. Predicted as columns and observed as rows.
#'
#' Copyright 2023 Battelle Energy Alliance ALL RIGHTS RESERVED
#'
#' @param observed (binary vector) The observed values for the response.
#' @param predicted (binary vector) The fitted values developed by the model.
#' @return confusion (integer data frame) The confusion matrix per class.
#'
#' @export confusion
confusion = function(observed, predicted) {
  unique_class = sort(unique(observed))
  class_len = length(unique_class)

  conf_mat = matrix(rep(0, class_len^2), ncol = class_len)
  colnames(conf_mat) = unique_class
  rownames(conf_mat) = unique_class
  conf_mat = as.data.frame(conf_mat)

  for (i in unique_class) {
    row_index = which(unique_class == i)
    obs_index = which(observed == i)
    pred_vals = table(predicted[obs_index])
    for (j in names(pred_vals)) {
      conf_mat[[j]][row_index] = pred_vals[[j]]
    }
  }

  conf_mat
}
