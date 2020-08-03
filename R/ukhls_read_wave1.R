
#' Read Understanding Society Wave 1
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
#' @param file Character - the file path and name.
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
ukhls_read_wave1 <- function(
  root = c("C:/"),
  file = "Users/User/Documents/Datasets/UKHLS/tab/ukhls_w1/a_indresp.tab"
) {


print("Reading UKHLS Wave 1")
  data <- data.table::fread(
    paste0(root[1], file),
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )

  data.table::setnames(data, names(data), tolower(names(data)))

  id_vars  <- colnames(data[ , c(1,2,3,6,7,9)])
  demographic_vars <- colnames(data[ , c(12,13,14,1303,1304,1400)])
  econ_stat_vars <- colnames(data[,c(23)])
  weight_vars <- colnames(data[,c(1398)])


  names <- c(id_vars,demographic_vars,econ_stat_vars,weight_vars)
  names <- tolower(names)

  data <- data[ , names, with = F]

  data.table::setnames(data,

                       c("pidp","a_hidp","a_pno","a_psu","a_strata","a_month",
                         ## demographic
                         "a_sex","a_dvage","a_birthy","a_gor_dv","a_urban_dv","a_racel_dv",
                         ## economic stauts
                         "a_jbstat",
                         ## weight
                         "a_indinus_xw"),

                       c("pidp","hidp","person_number","psu","strata","sample_month",
                         ## demographic
                         "sex","age","birth_year","region","urban","ethnicity_raw",
                         ## economic status
                         "econ_stat",
                         ## weight
                         "weight"))

  data$wave <- 1

  return(data[])
}
