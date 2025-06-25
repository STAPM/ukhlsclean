#' Clean Income Variables
#'
#' Reads and does basic cleaning on the UKHLS income variables. Total income (gross and net),
#' division of net income into labour income, miscellaneous income, private benefit income,
#' investment income, pension income, social benefit income.
#'
#' @param data Data table. Understanding Society data produced using the read functions.
#'
#' @export
ukhls_clean_income <- function(data = NULL) {


  ##################
  ## RETAIN THE CLEANED VARIABLES

  final_data <- data[, c("pidp", "id", "hidp", "wave_no",
                         "fimnnet_dv", "fimngrs_dv",
                         "fimnlabnet_dv", "fimnmisc_dv", "fimnprben_dv",
                         "fimninvnet_dv", "fimnpen_dv", "fimnsben_dv")]


  var_names <- c("fimnnet_dv", "fimngrs_dv",
                 "fimnlabnet_dv", "fimnmisc_dv", "fimnprben_dv",
                 "fimninvnet_dv", "fimnpen_dv", "fimnsben_dv")

  setnames(final_data, var_names, paste0("y_", var_names))

  return(final_data)
}
