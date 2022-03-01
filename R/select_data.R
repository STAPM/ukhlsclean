#' Select variables and apply filters
#'
#' Selects the variables required for analysis and selects only the rows without missing data
#' for specified variables.
#'
#' @param data Data table - the Health Survey for England dataset.
#' @param ages Integer vector - the ages in single years to retain (defaults to 16 to 89 years).
#' @param waves Integer vector - the waves of the UKHLS to retain (defaults to all - 1 to 11).
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
#' data <- select_data(data, keep_vars = c("age", "gender", "region"))
#'
#' }
#'

select_data <- function(
  data,
  ages = 16:89,
  waves = 1:11,
  keep_vars = NULL,
  complete_vars = c("age")
) {

  ### apply age and wave filters

  data <- data[age %in% ages & wave_no %in% waves]

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


  return(data[])
}
