% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ImportFunctions.R, R/PreprocessingFunction.R
\docType{data}
\name{date_pull}
\alias{date_pull}
\title{Shared pre-processing of input CSV files}
\format{
An object of class \code{Date} of length 1.

An object of class \code{Date} of length 1.
}
\usage{
date_pull

date_pull
}
\arguments{
\item{file.name}{Path of the data file (CDISC format)}

\item{excluded.columns}{Columns to be removed}

\item{dtplyr.step}{Return the output as \code{dtplyr_step} to avoid unnecessary future calls to \code{as_tibble} or \code{as.data.table}}

\item{input.tbl}{Input tibble (output of \code{process.all.data})}
}
\value{
The contents of \code{file.name} as a tibble or \code{dtplyr_step}

A \code{tibble} intended for input into other aggregation functions (e.g. \code{age.pyramid.prep})
}
\description{
Shared pre-processing of input CSV files

Preprocessing step for all aggregations. Currently: remaps outcome to death, discharge or NA, cuts age into 5-year age groups, and adds a year-epiweek column
}
\keyword{datasets}
\keyword{internal}
