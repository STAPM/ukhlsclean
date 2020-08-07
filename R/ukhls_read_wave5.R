
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
ukhls_read_wave5 <- function(
  root = c("C:/"),
  file = "Users/User/Documents/Datasets/UKHLS/tab/ukhls_w5/e_indresp.tab"
) {


  print("Reading UKHLS Wave 5")
  data <- data.table::fread(
    paste0(root[1], file),
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )

  data.table::setnames(data, names(data), tolower(names(data)))

  id_vars  <- colnames(data[ , c(1,2,3,4,8,9,11)])
  demographic_vars <- colnames(data[ , c(14,15,16,2497,2498,197)])
  econ_stat_vars <- colnames(data[,c(112)])
  smoke_vars <- colnames(data[,c(891,889,890,892,893,894,895)])
  weight_vars <- colnames(data[,c(2602)])


  names <- c(id_vars,demographic_vars,econ_stat_vars,weight_vars)
  names <- tolower(names)

  data <- data[ , names, with = F]

  data.table::setnames(data,

                       c("pidp","pid","e_hidp","e_pno","e_psu","e_strata","e_month",
                         ## demographic
                         "e_sex","e_dvage","e_birthy","e_gor_dv","e_urban_dv","e_racel",
                         ## economic stauts
                         "e_jbstat",
                         ## weight
                         "e_indpxub_lw"),

                       c("pidp","pid","hidp","person_number","psu","strata","sample_month",
                         ## demographic
                         "sex","age","birth_year","region","urban","ethnicity_raw",
                         ## economic status
                         "econ_stat",
                         ## weight
                         "weight"))

  data$wave <- 5

  data$bhps <- ifelse(is.na(data$pid),TRUE,FALSE)

  return(data[])
}
