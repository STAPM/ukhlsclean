% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ukhls_clean_global.R
\name{ukhls_clean_global}
\alias{ukhls_clean_global}
\title{Clean UKHLS data}
\usage{
ukhls_clean_global(
  data,
  ages = 16:89,
  country = "UK",
  keep_vars = NULL,
  complete_vars = c("year", "age", "sex"),
  calendar_year
)
}
\arguments{
\item{data}{Data table - the combined Understanding Society dataset for one wave.}

\item{ages}{Integer vector - the ages in single years to retain (defaults to 16 to 89 years).}

\item{country}{Character - country to produce data for. One of c("UK","england","wales","scotland","northern_ireland"). Defaults to all UK.}

\item{keep_vars}{Character vector - the names of the variables to keep (defaults to NULL - retaining all variables).}

\item{complete_vars}{Character vector - the names of the variables on which the selection of
complete cases will be based (defaults to year, age and sex).}

\item{calendar_year}{Logical - TRUE when the code is processing calendar year data and merges in ONS population counts data.}
}
\value{
Returns a new set of variables
}
\description{
A wrapper function for applying all of the cleaning functions, selecting the
desired variables/observations for the analysis, and specifying complete case
restrictions.
}
