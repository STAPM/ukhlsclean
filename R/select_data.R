#' Select variables and apply filters
#'
#' Selects the variables required for analysis and selects only the rows without missing data
#' for specified variables.
#'
#' @param data Data table - the cleaned Understanding Society dataset.
#' @param ages Integer vector - the ages in single years to retain (defaults to 16 to 89 years).
#' @param keep_vars Character vector - the names of the variables to keep (defaults to year and age).
#' @param complete_vars Character vector - the names of the variables on which the selection of complete cases will be based (defaults to year and age).
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
  keep_vars = NULL,
  complete_vars = c("age")
) {

  ### apply age and wave filters

  data <- data[d_age %in% ages]

  ## keep only complete cases of variables named in complete_vars

  for(cv in complete_vars) {

    data <- data[!is.na(get(cv))]

  }

  ## only keep variables named in keep_vars

  if (is.null(keep_vars)) {

    keep_vars <- names(data)
  }

  keep_vars <- intersect(names(data), keep_vars)

  data <- data[ , keep_vars, with = F]


  return(data)
}
