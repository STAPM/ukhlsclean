#' Select variables and apply filters
#'
#' Selects the variables required for analysis and selects only the rows without missing data
#' for specified variables.
#'
#' @param data Data table - the cleaned Understanding Society dataset.
#' @param ages Integer vector - the ages in single years to retain (defaults to 16 to 89 years).
#' @param country Character - country to produce data for. One of c("UK","england","wales","scotland","northern_ireland"). Defaults to all UK.
#' @param keep_vars Character vector - the names of the variables to keep (defaults to year and age).
#' @param complete_vars Character vector - the names of the variables on which the selection of complete cases will be based (defaults to year and age).
#' @param calendar_year Logical - TRUE when the code is processing calendar year data and merges in ONS population counts data.
#' @importFrom data.table :=
#' @return Returns a reduced version of data
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#'
#' }
#'
select_data <- function(
  data,
  ages = 16:89,
  country = "UK",
  keep_vars = NULL,
  complete_vars = c("age"),
  calendar_year
) {

  ### apply age and country filters

  data <- data[d_age %in% ages]

  if (country == "UK"){

  }
  if (country == "england") {

    data <- data[d_country == "england",]
  }
  if (country == "scotland") {

    data <- data[d_country == "scotland",]
  }
  if (country == "wales") {

    data <- data[d_country == "wales",]
  }
  if (country == "northern_ireland") {

    data <- data[d_country == "northern_ireland",]
  }




  ## keep only complete cases of variables named in complete_vars

  for(cv in complete_vars) {

    data <- data[!is.na(get(cv))]

  }

  #########################################################################
  ## only keep variables named in keep_vars plus mandatory identifier and
  ## weight variables

  if (is.null(keep_vars)) {

    keep_vars <- names(data)
  }
  if (!is.null(keep_vars) & calendar_year == TRUE) {

    keep_vars <- union(c("id","hidp","wave","wave_no","bhps_sample",
                         "year","month","day","weight_xw","pop_factor"), keep_vars)
  }
  if (!is.null(keep_vars) & calendar_year == FALSE) {

    keep_vars <- union(c("id","hidp","wave","wave_no","bhps_sample",
                         "year","month","day","weight_xw"), keep_vars)
  }

  keep_vars <- intersect(names(data), keep_vars)

  data <- data[ , keep_vars, with = F]


  return(data)
}
