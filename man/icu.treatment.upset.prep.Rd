% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AggregationFunctions.R
\name{icu.treatment.upset.prep}
\alias{icu.treatment.upset.prep}
\title{Aggregate data for ICU treatments upset plot}
\usage{
icu.treatment.upset.prep(input.tbl, max.treatments = 5)
}
\arguments{
\item{input.tbl}{Input tibble (output of \code{data.preprocessing})}

\item{max.treatments}{The plot will display only the n most common treatments, this parameter is n}
}
\value{
A \code{tibble} containing the input data for the ICU treatments upset plot
}
\description{
Aggregate data for ICU treatments upset plot
}
