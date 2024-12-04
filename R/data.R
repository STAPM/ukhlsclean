#' Monthly Consumer Price Index
#'
#' Returns the CPIH inflation index on a monthly basis with a base month of January 2024 Data are obtained from the
#' \href{https://www.ons.gov.uk/economy/inflationandpriceindices/timeseries/l522/mm23}{ONS website}
#' and the data were last updated 04-Dec-2024. Data are available up to Oct-2024.
#'
#' @format A data frame with 442 observations and 3 variables.
#' \describe{
#'     \item{year}{}
#'     \item{month}{}
#'     \item{index}{}
#' }
"cpih"


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
#' mid-year population estimates. Stratified by calendar year (2020 and 2021) UK nation, single year of age, and sex.
#'
#' @source \href{https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationestimatesforukenglandandwalesscotlandandnorthernireland}{ONS mid-year population estimates}
#'
"PopulationCounts"

#' Monthly Retail Price Index
#'
#' Returns the RPI inflation index on a monthly basis with a base month of January 2024 Data are obtained from the
#' \href{https://www.ons.gov.uk/generator?format=xls&uri=/economy/inflationandpriceindices/timeseries/cdko/mm23}{ONS website}
#' and the data were last updated 01-Dec-2024. Data are available up to Oct-2024.
#'
#' @format A data frame with 929 observations and 3 variables.
#' \describe{
#'     \item{year}{}
#'     \item{month}{}
#'     \item{index}{}
#' }
"rpi"
