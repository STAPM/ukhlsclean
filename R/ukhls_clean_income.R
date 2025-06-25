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

  data[, income_net := fimnnet_dv]
  data[, income_grs := fimngrs_dv]

  data[, lab_income_net  := fimnlabnet_dv]
  data[, misc_income_net := fimnmisc_dv]
  data[, priv_income_net := fimnprben_dv]
  data[, inv_income_net  := fimninvnet_dv]
  data[, pens_income_net := fimnpen_dv]
  data[, ben_income_net  := fimnsben_dv]


  ##################
  ## RETAIN THE CLEANED VARIABLES

  final_data <- data[, c("pidp", "id", "hidp", "wave_no",
                         "income_net", "income_grs",
                         "lab_income_net", "misc_income_net", "priv_income_net",
                         "inv_income_net", "pens_income_net", "ben_income_net")]


  var_names <- c("income_net", "income_grs",
                 "lab_income_net", "misc_income_net", "priv_income_net",
                 "inv_income_net", "pens_income_net", "ben_income_net")

  setnames(final_data, var_names, paste0("y_", var_names))

  return(final_data)
}
