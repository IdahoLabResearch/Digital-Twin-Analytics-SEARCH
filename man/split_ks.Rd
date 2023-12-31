% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/split_ks.R
\name{split_ks}
\alias{split_ks}
\title{Split KS}
\usage{
split_ks(x, train_percent = 0.8)
}
\arguments{
\item{x}{(data frame or matrix) A set of independent variables.}

\item{train_percent}{(scalar | optional) The percent of values in the
training set.}
}
\value{
split (list vector) The split of x into training and test set
indices.
}
\description{
Wrapper for splitting the data via Kennard-Stone algorithm.
}
\details{
Copyright 2023 Battelle Energy Alliance ALL RIGHTS RESERVED
}
