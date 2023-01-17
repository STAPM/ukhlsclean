#' Monthly Consumer Price Index
#'
#' Returns the CPI inflation index on a monthly basis (currently set to
#' re-base the index to March 2020, the most recent month) - data are obtained from the
#' \href{https://www.ons.gov.uk/economy/inflationandpriceindices/timeseries/l522/mm23}{ONS website}
#' last accessed 19/06/2020.
#'
#' @format A data frame with 351 observations and 3 variables.
#' \describe{
#'     \item{year}{}
#'     \item{month}{}
#'     \item{cpi_value}{}
#' }
"cpi"

#' EQ-5D Coefficient Matrix
#'
#' Matrix of coefficients used to calculate EQ-5D domains from the SF-12.
#' \href{https://www.herc.ox.ac.uk/downloads/downloads-supporting-material-1/sf-12-responses-and-eq-5d-utility-values}{SF-12 Responses and EQ-5D Utility Values}
#'
"CoefficientMatrix"

#' Population Counts
#'
#' Understanding Society weights are scaled to a mean of 1 among the relevant sample. Population count data can be used with the
#' calendar year datasets to scale up totals to the population. Data are obtained from the Office for National Statistics
#' mid-year population estimates. Stratified by UK nation, single year of age, and sex.
#'
#' @source \href{https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationestimatesforukenglandandwalesscotlandandnorthernireland}{ONS mid-year population estimates}
#'
"PopulationCounts"
