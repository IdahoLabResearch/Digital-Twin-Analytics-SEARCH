# Copyright 2023 Battelle Energy Alliance ALL RIGHTS RESERVED
#' @rdname confirm
#' @export
confirm.confirmClass = function(x, ...) {
  class_prediction_values = confusionValues(x$observed, x$predicted)
  for (conf_val in names(class_prediction_values)) {
    results = list()
    data = class_prediction_values[[conf_val]]
    results$ppv = data$tp / (data$tp + data$fp)
    results$npv = data$tn / (data$tn + data$fn)
    results$tpr = data$tp / (data$tp + data$fn)
    results$tnr = data$tn / (data$tn + data$fp)
    results$fnr = data$fn / (data$fn + data$tp)
    results$fpr = data$fp / (data$fp + data$tn)
    results$fdr = data$fp / (data$fp + data$tp)
    results$fomr = data$fn / (data$fn + data$tn)
    results$accuracy = (data$tp + data$tn) / (data$fn + data$fp +
                                                data$tp + data$tn)
    results$f1 = results$ppv * results$tpr / (results$ppv + results$tpr)
    class_prediction_values[[conf_val]] = results
  }

  class_prediction_values
}
