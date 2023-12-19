# Copyright 2023 Battelle Energy Alliance ALL RIGHTS RESERVED
r_repo = "https://ftp.osuosl.org/pub/cran/"

util_packages = c("devtools", "testthat", "xml2", "roxygen2", "reticulate",
                  "DT", "shinythemes", "shiny", "plotly", "ggplot2", "ggdendro")
install.packages(util_packages, repos = r_repo)

devtools::install_git("https://github.com/IdahoLabResearch/Digital-Twin-Analytics-SEARCH",
                      dependencies = T, repos = r_repo)
search::genPythonEnv()
