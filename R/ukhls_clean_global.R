#' Clean UKHLS data
#'
#' A wrapper function for applying all of the cleaning functions, selecting the
#' desired variables/observations for the analysis, and specifying complete case
#' restrictions.
#'
#' @param data Data table - the combined Understanding Society dataset.
#' @param ages Integer vector - the ages in single years to retain (defaults to 16 to 89 years).
#' @param waves Integer vector - the waves of the UKHLS to retain (defaults to all - 1 to 11).
#' @param keep_vars Character vector - the names of the variables to keep (defaults to NULL - retaining all variables).
#' @param complete_vars Character vector - the names of the variables on which the selection of complete cases will be based (defaults to year, age and gender).
#' @return Returns a new set of variables
#' @export
ukhls_clean_global <- function(data,
                               ages = 16:89,
                               waves = 1:11,
                               keep_vars = NULL,
                               complete_vars = c("year", "age", "sex")
) {

  ## fix bug that occurs if age is not in keep_vars

  if (!("age" %in% names(data)) ) {
    ages = NULL
  }

  ## run the cleaning functions

  data %<>%
    clean_demographic %>%
    clean_health %>%
    clean_alcohol %>%
    clean_smoke %>%
    clean_econ_status %>%
    clean_hhold %>%
    clean_hours_earn %>%

  ukhlsclean::select_data(
    ages = ages,
    waves = waves,
    keep_vars = keep_vars,
    complete_vars = complete_vars
  )


  return(data)
}
