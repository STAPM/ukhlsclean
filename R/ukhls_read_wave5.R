
#' Read Understanding Society Wave 5
#'
#' Reads and does basic cleaning on the UKHLS first wave.
#'
#' A sample of the population living in private households. All persons living in the house, including those
#' under 2 years were eligible for inclusion. At addresses where there were more than two children under 16,
#' two children were selected at random. Information was obtained directly from persons aged 13 and
#' over. Information about children aged 0-12 was obtained from a parent, with the child present.
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
ukhls_read_wave5 <- function(
  root = c("C:/"),
  path = "Users/User/Documents/Datasets/UKHLS/tab/"
) {


  print("Reading UKHLS Wave 5")
  data <- data.table::fread(
    paste0(root[1], path, "ukhls_w5/e_indresp.tab"),
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )

  data.table::setnames(data, names(data), tolower(names(data)))

  id_vars  <- colnames(data[ , c(1,2,3,4,8,9)])
  demographic_vars <- colnames(data[ , c(14,15,16,2497,2498)])
  econ_stat_vars <- colnames(data[,c(112)])
  smoke_vars <- colnames(data[,c(891,889,890,892,893,894,895)])
  weight_vars <- colnames(data[,c(2602)])


  names <- c(id_vars,demographic_vars,econ_stat_vars,smoke_vars,weight_vars)
  names <- tolower(names)

  data <- data[ , names, with = F]

  data.table::setnames(data,

                       c("pidp","pid","e_hidp","e_pno","e_psu","e_strata",
                         ## demographic
                         "e_sex","e_dvage","e_birthy","e_gor_dv","e_urban_dv",
                         ## economic stauts
                         "e_jbstat",
                         ## smoking variables
                         "e_ncigs","e_smever","e_smnow","e_smcigs","e_smncigs","e_aglquit","e_smagbg",
                         ## weight
                         "e_indpxub_lw"),

                       c("pidp","pid","hidp","person_number","psu","strata",
                         ## demographic
                         "sex","age","birth_year","region","urban",
                         ## economic status
                         "econ_stat",
                         ## smoking variables
                         "ncigs","smever","smnow","smcigs","smncigs","aglquit","smagbg",
                         ## weight
                         "weight"))

  data$wave <- 5

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
