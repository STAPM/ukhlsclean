#' Clean UKHLS data
#'
#' A wrapper function for applying all of the cleaning functions, selecting the
#' desired variables/observations for the analysis, and specifying complete case
#' restrictions.
#'
#' @param data Data table - the combined Understanding Society dataset.
#' @param ages Integer vector - the ages in single years to retain (defaults to 16 to 89 years).
#' @param keep_vars Character vector - the names of the variables to keep (defaults to NULL - retaining all variables).
#' @param complete_vars Character vector - the names of the variables on which the selection of complete cases will be based (defaults to year, age and gender).
#' @return Returns a new set of variables
#' @export
ukhls_clean_global <- function(data,
                               ages = 16:89,
                               keep_vars = NULL,
                               complete_vars = c("year", "age", "gender")
) {

  data <- data %>%
    clean_demographic() %>%
    clean_health() %>%
    clean_alcohol() %>%
    clean_smoke() %>%
    clean_econ_status() %>%
    clean_hhold() %>%
    clean_hours_earn()

  ## if keep_vars argument is NULL, keep all variables
  if (is.null(keep_vars)) {
  keep_vars <- names(data)
  } else if (!is.null(keep_vars))
  keep_vars <- intersect(names(data), keep_vars)

  data <- data[ , keep_vars, with = F]

  ## only keep cases where all variables in complete_vars are non-missing
  for(cv in complete_vars) {
    data <- data[!is.na(get(cv))]
  }

  data <- data[age %in% ages]


  return(data)
}
