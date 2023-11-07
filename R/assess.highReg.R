# Copyright 2023 Battelle Energy Alliance ALL RIGHTS RESERVED
#' @rdname assess
#' @export
assess.highReg = function(x, ...) {
  result = list()

  result$PCR = regPCR(x)
  result$ENET = regENET(x)
  result$RIDGE = regENET(x, alpha = 0)
  result$LASSO = regENET(x, alpha = 1)
  result$RF = regRF(x)
  result$SVM = regSVM(x)
  result$MLP = regMLP(x)

  result
}
