#' Generate the Python environment for reticulate
#'
#' A helper function to generate the required Python environment through
#' reticulate.
#'
#' Copyright 2023 Battelle Energy Alliance ALL RIGHTS RESERVED
#'
#' @return NULL
#'
#' @import reticulate
#' @export genPythonEnv
genPythonEnv = function() {
  python_depends = c("numpy", "pandas", "scikit-learn")

  install_miniconda(force = TRUE)
  conda_create("search")
  use_condaenv("search")
  py_install(python_depends)

  return()
}
