#' Read Understanding Society - Covid-19 Data Wave 1
#'
#' Reads and does basic cleaning on the UKHLS Covid-19 survey first wave.
#'
#' A sample of the UKHLS respondents repeatedly sampled during Covid-19 pandemic.
#'
#' MISSING VALUES
#'
#' \itemize{
#' \item -1 Don't know.
#' \item -2 Refused: Used only for variables on the nurse schedules, this code indicates that a
#' respondent refused a particular measurement or test or the measurement was attempted but not
#' obtained or not attempted.
#' \item -8 Not applicable: Used to signify that a particular variable did not apply to a given respondent
#' usually because of internal routing. For example, men in women only questions.
#' \item -9 Missing
#' }
#'
#' @param root Character - the root directory.
#' @param path Character - the file path and name.
#' @importFrom data.table :=
#' @return Returns a data table. Note that:
#' \itemize{
#' \item Missing data ("NA", "", "-1", "-2", "-6", "-7", "-9", "-90", "-90.0", "N/A") is replace with NA,
#' except -8 ("don't know") as this is data.
#' \item All variable names are converted to lower case.
#' \item Each data point is assigned a weight of 1 as there is no weight variable supplied.
#' \item A single sampling cluster is assigned.
#' \item The probabilistic sampling unit have the year appended to them.
#' }
#' @export
ukhls_read_covid19_1 <- function(
  root = c("C:/"),
  path = "Users/User/Documents/Datasets/UKHLS/tab/"
) {
  print("Reading UKHLS Covid-19 Wave 1")

  #### First Covid Wave
    data <- data.table::fread(
      paste0(root[1], path, "ukhls_covid19/ca_indresp_w.tab"),
      na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
    )

  ##### Clean wave 1

  data.table::setnames(data, names(data), tolower(names(data)))

  id_vars  <- Hmisc::Cs(pidp,ca_betaindin_xw_t)
  alc_vars <- Hmisc::Cs(ca_auditc1_cv,ca_auditc2,ca_auditc3_cv,ca_auditc4,ca_auditc5_cv)
  smk_vars <- Hmisc::Cs(ca_smoker,ca_ncigs,ca_ecigs1)


  names <- c(id_vars,alc_vars,smk_vars)
  data <- data[ , names, with = F]

  data.table::setnames(data,

                       c("pidp","ca_betaindin_xw_t",
                         ### alcohol vars
                         "ca_auditc1_cv","ca_auditc2","ca_auditc3_cv","ca_auditc4","ca_auditc5_cv",
                         ### smoker vars
                         "ca_smoker","ca_ncigs","ca_ecigs1"
                         ),

                       c("id","weight",
                         ### alcohol vars
                         "auditc1","auditc2","auditc3","auditc4","auditc5",
                         ### smoker vars
                         "smoker","ncigs","ecigs1"
                         ))

  data$wave <- "UKHLS Covid-19 Wave 1"
  data$wave_no <- 1


  return(data[])
}
