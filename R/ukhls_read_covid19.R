#' Read Understanding Society - Covid-19 Data
#'
#' Reads and does basic cleaning on the UKHLS Covid-19 waves.
#'
#' A sample of the UKHLS repeatedly sampled during Covid-19 pandemic.
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
ukhls_read_covid19 <- function(
  root = c("C:/"),
  path = "Users/User/Documents/Datasets/UKHLS/tab/"
) {
  print("Reading UKHLS Covid-19 Waves")

  #### First Covid Wave
    covid.w1 <- data.table::fread(
      paste0(root[1], path, "ukhls_covid19/ca_indresp_w.tab"),
      na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  #### Second Covid Wave
  covid.w2 <- data.table::fread(
    paste0(root[1], path, "ukhls_covid19/cb_indresp_w.tab"),
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  #### Third Covid Wave
  covid.w3 <- data.table::fread(
    paste0(root[1], path, "ukhls_covid19/cc_indresp_w.tab"),
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )

  ##### Clean wave 1
  data <- covid.w1

  data.table::setnames(data, names(data), tolower(names(data)))

  id_vars  <- colnames(data[ , c(1:3)])
  demographic_vars <- colnames(data[ , c(4:6,14,15,17:22)])
  corona_vars <- colnames(data[,c(23:39)])
  health_vars <- colnames(data[,c(40:62,147)])
  care_vars <- colnames(data[,c(99:146)])
  work_vars_bl <- colnames(data[,c(148:156)])
  work_vars <- colnames(data[,c(157:202)])


  names <- c(id_vars,demographic_vars,econ_stat_vars,smoke_vars,weight_vars)
  names <- tolower(names)

  data <- data[ , names, with = F]

  data.table::setnames(data,

                       c("pidp","pid","i_hidp","i_pno","i_psu","i_strata",
                         ## demographic
                         "i_sex","i_dvage","i_birthy","i_gor_dv","i_urban_dv",
                         ## economic stauts
                         "i_jbstat",
                         ## smoke variables
                         "i_smoker", "i_ncigs", "i_giveup", "i_ecigs1",
                         ## weight
                         "i_indpxub_lw"),

                       c("pidp","pid","hidp","person_number","psu","strata",
                         ## demographic
                         "sex","age","birth_year","region","urban",
                         ## economic status
                         "econ_stat",
                         ## smoke variables
                         "smoker", "ncigs", "giveup", "ecigs1",
                         ## weight
                         "weight"))

  data$wave <- 9

  data$bhps <- ifelse(!is.na(data$pid),TRUE,FALSE)

  ######## Add in cross-wave data

  data.xwave <- data.table::fread(
    paste0(root[1], path, "ukhls_wx/xwavedat.tab"),
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(data.xwave, names(data.xwave), tolower(names(data.xwave)))

  xwave_vars  <- colnames(data.xwave[ , c(1,34)])

  data.xwave <- data.xwave[ , xwave_vars, with = F]
  data.table::setnames(data.xwave,
                       # old names
                       c("pidp","racel_dv"),
                       # new names
                       c("pidp","ethnicity_raw"))

  ####### Combine - keep all observations in the main data and drop excess xwave observations

  data_merged <- merge(x = data,
                       y = data.xwave,
                       by="pidp",
                       all.x=TRUE,
                       all.y=FALSE)

  return(data_merged[])
}
