% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ukhls_clean_hhold.R
\name{ukhls_clean_hhold}
\alias{ukhls_clean_hhold}
\title{Clean Household Variables}
\usage{
ukhls_clean_hhold(
  data = NULL,
  calendar_year = FALSE,
  inflation = ukhlsclean::cpih
)
}
\arguments{
\item{data}{Data table. Understanding Society data produced using the read functions.}

\item{calendar_year}{Logical - TRUE when the code is processing calendar year data (defaults to FALSE).}

\item{inflation}{Data table. Inflation data input for real-terms adjustments. Defaults to CPIH.}
}
\description{
Clean all household level variables; household type (composition of adults and children),
household size, ownership status, and age of the youngest child in the household.
}
