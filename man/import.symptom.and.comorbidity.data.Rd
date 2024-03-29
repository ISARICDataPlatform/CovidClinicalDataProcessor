% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ImportFunctions.R, R/ImportFunctionsModular.R
\name{import.symptom.and.comorbidity.data}
\alias{import.symptom.and.comorbidity.data}
\title{Import data on symptoms and comorbidities}
\usage{
import.symptom.and.comorbidity.data(file.name, dtplyr.step = TRUE)

import.symptom.and.comorbidity.data(file.name, dtplyr.step = TRUE)
}
\arguments{
\item{file.name}{Path of the symptoms data file (CDISC format)}

\item{dtplyr.step}{Return the output as \code{dtplyr_step} to avoid unnecessary future calls to \code{as_tibble} or \code{as.data.table}}
}
\value{
Formatted comorbidity and symptom data as a tibble or \code{dtplyr_step}

Formatted comorbidity and symptom data as a tibble or \code{dtplyr_step}
}
\description{
Import data on symptoms and comorbidities

Import data on symptoms and comorbidities
}
