#' Clean Youth data Variables
#'
#' Produce clean versions of variables measuring youth smoking outcomes.
#'
#' @param data Data table. Understanding Society data produced using the read functions.
#'
#' @export
ukhls_clean_youth <- function(youth_data = NULL) {

  # youth_data <- ukhls_read_youth()

  youth_data <- youth_data[order(pidp),]

  youth_data <- youth_data[, c("pidp","age","ypevrsmo","ypsmofrq")]

  ####

  youth_data[ypevrsmo == 1 & ypsmofrq > 2, current := 1]
  youth_data[ypevrsmo == 1 & ypsmofrq <= 2, current := 2]
  youth_data[ypevrsmo == 2, current := 2]

  youth_data[is.na(ypevrsmo), current := NA_integer_]

  # youth_data[ypevrsmo == 1 & is.na(ypsmofrq), ] ### Missing values for current - unsure what to do

  # eversmoke <- copy(youth_data)

  youth_data[, ypevrsmo := NULL]
  youth_data[, ypsmofrq := NULL]
  youth_data[, age := paste0("curr.smk_",age)]

  youth_data <- youth_data[, .(current = suppressWarnings(min(current, na.rm = TRUE))), by = c("pidp","age")]
  youth_data <- youth_data[current == Inf, current := NA_integer_]

  youth_data[, current := as.character(current)]
  youth_data[current == "2", current := "No"]
  youth_data[current == "1", current := "Yes"]

  youth_data <- dcast(youth_data,
                   pidp ~ age,
                   value.var = "current")

  # #### ever smoke indicator
  #
  # eversmoke[, ypsmofrq := NULL]
  # eversmoke[, age := paste0(age,"yro")]
  #
  # eversmoke.m1 <- dcast(eversmoke,
  #                       id ~ age,
  #                       value.var = "ypevrsmo",
  #                       fill = NA,
  #                       fun.aggregate = NULL)
  # unique(eversmoke.m1$`10yro`)

  setnames(youth_data,
           "pidp",
           "id")

  final_data <- youth_data[, c("id",
                         "curr.smk_10", "curr.smk_11", "curr.smk_12", "curr.smk_13", "curr.smk_14", "curr.smk_15", "curr.smk_16")]

  var_names <- c("curr.smk_10", "curr.smk_11", "curr.smk_12", "curr.smk_13", "curr.smk_14", "curr.smk_15", "curr.smk_16")

  setnames(final_data, var_names, paste0("yp_", var_names))

  return(final_data)
}
