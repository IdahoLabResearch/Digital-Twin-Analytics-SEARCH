# Copyright 2023 Battelle Energy Alliance ALL RIGHTS RESERVED
#' @rdname assess
#' @export
assess.lowReg = function(x, ...) {
  result = list()
  result$LM = regLM(x)
  result$GAM = regGAM(x)

  class(x) = "highReg"
  result = c(result, assess(x))

  result
}
