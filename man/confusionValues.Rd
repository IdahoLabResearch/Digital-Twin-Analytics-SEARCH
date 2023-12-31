% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/confusionValues.R
\name{confusionValues}
\alias{confusionValues}
\title{Confusion values}
\usage{
confusionValues(observed, predicted)
}
\arguments{
\item{observed}{(binary vector) The observed values for the response.}

\item{predicted}{(binary vector) The fitted values developed by the model.}
}
\value{
confusion_values (list) The false negative, false positive,
true negative, and true positive values for each class.
}
\description{
The false negative, false positive, true negative, and true positive
values for each class.
}
\details{
Copyright 2023 Battelle Energy Alliance ALL RIGHTS RESERVED
}
