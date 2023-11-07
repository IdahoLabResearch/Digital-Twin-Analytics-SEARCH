# Copyright 2023 Battelle Energy Alliance ALL RIGHTS RESERVED
#' @rdname confirm
#' @export
confirm.confirmReg = function(x, ...) {
  results = list()
  results$r2 = r2(observed = x$observed, predicted = x$predicted)
  results$rmse = rmse(observed = x$observed, predicted = x$predicted)
  results$mape = mape(observed = x$observed, predicted = x$predicted)
  results$smape = smape(observed = x$observed, predicted = x$predicted)

  results
}
