% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ukhls_read_wave1.R
\name{ukhls_read_wave1}
\alias{ukhls_read_wave1}
\title{Read Understanding Society Wave 1}
\source{
University of Essex, Institute for Social and Economic Research. (2022). Understanding Society: Waves 1-12, 2009-2021
and Harmonised BHPS: Waves 1-18, 1991-2009. [data collection]. 17th Edition. UK Data Service. SN: 6614,
\href{https://beta.ukdataservice.ac.uk/datacatalogue/studies/study?id=6614}{DOI: 10.5255/UKDA-SN-6614-18}
}
\usage{
ukhls_read_wave1(
  root = c("X:/"),
  file = "HAR_PR/PR/USoc/Data/SN6614_2024_12_02/tab/ukhls/",
  full = TRUE
)
}
\arguments{
\item{root}{Character - the root directory.}

\item{file}{Character - the file path and name.}

\item{full}{Logical - TRUE if restricting the sample to full interviews only (excluding proxies)}
}
\value{
Returns a data table. Note that:
\itemize{
\item Missing data ("NA", "", "-1", "-2", "-6", "-7", "-9", "-90", "-90.0", "N/A") is replace with NA,
except -8 ("don't know") as this is data.
\item All variable names are converted to lower case.
\item Each data point is assigned a weight of 1 as there is no weight variable supplied.
\item A single sampling cluster is assigned.
\item The probabilistic sampling unit have the year appended to them.
}
}
\description{
Reads and performs basic cleaning operations on the UKHLS first wave. Missing values as detailed below are all set to NA.
}
\details{
A sample of the population living in private households. All persons living in the household, including those
under 2 years were eligible for inclusion. At addresses where there were more than two children under 16,
two children were selected at random. Information was obtained directly from persons aged 13 and
over. Information about children aged 0-12 was obtained from a parent, with the child present.

MISSING VALUES

\itemize{
\item -1 Don't know. When the respondent does not know the answer to a question.
\item -2 Refused: When the respondent refuses to answer a question.
\item -7 Proxy: A question not included in the subset of questions asked of proxy respondents.
obtained or not attempted.
\item -8 Not applicable: Used to signify that a particular variable did not apply to a given respondent
usually because of internal routing. For example, men in women only questions.
\item -9 Missing by error or implausible answer.
}
}
