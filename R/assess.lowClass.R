# Copyright 2023 Battelle Energy Alliance ALL RIGHTS RESERVED
#' @rdname assess
#' @export
assess.lowClass = function(x, ...) {
  result = list()
  result$LOGISTIC = classLOGISTIC(x)
  result$GAM = classGAM(x)

  class(x) = "highClass"
  result = c(result, assess(x))

  result
}
