% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predict.regSVM.R
\name{predict.regSVM}
\alias{predict.regSVM}
\title{Predict regSVM}
\usage{
\method{predict}{regSVM}(object, x, ...)
}
\arguments{
\item{object}{(object) A fit object.}

\item{x}{(list) A list containing [x], the set of independent variables and
[y], the response.}

\item{...}{Additional arguments.}
}
\value{
predicted (list of scalar vectors) The predicted values of train and
test.
}
\description{
Create an initial set of zeros for the train and test prediction.
}
\details{
Copyright 2023 Battelle Energy Alliance ALL RIGHTS RESERVED
}
