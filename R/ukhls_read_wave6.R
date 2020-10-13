#' Read Understanding Society Wave 6
#'
#' Reads and does basic cleaning on the UKHLS sixth wave.
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
ukhls_read_wave6 <- function(
  root = c("C:/"),
  path = "Users/User/Documents/Datasets/UKHLS/tab/"
) {


  print("Reading UKHLS Wave 6")
  data <- data.table::fread(
    paste0(root[1], path, "ukhls_w6/f_indresp.tab"),
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-10", "-90", "-90.0", "N/A")
  )

  data.table::setnames(data, names(data), tolower(names(data)))

  id_vars          <- colnames(data[,c(1,2,3,4,8,9)])
  demographic_vars <- colnames(data[,c(15,16,17,1931,1932)])
  econ_stat_vars   <- colnames(data[,c(57)])
  work_vars        <- colnames(data[,c(1836,1837,1012,1826)])
  education_vars   <- colnames(data[,c(1969)])
  health_vars      <- colnames(data[,c(434,517)])
  smoke_vars       <- colnames(data[,c(515,516)])
  weight_vars      <- colnames(data[,c(2047)])


  names <- c(id_vars,demographic_vars,econ_stat_vars,work_vars,education_vars,health_vars,smoke_vars,weight_vars)
  names <- tolower(names)

  data <- data[ , names, with = F]

  data.table::setnames(data,

                       c("pidp","pid","f_hidp","f_pno","f_psu","f_strata",
                         ## demographic
                         "f_sex","f_dvage","f_birthy","f_gor_dv","f_urban_dv",
                         ## economic stauts
                         "f_jbstat",
                         ## work variables
                         "f_paygu_dv","f_payg_dv","f_jbhrs","f_fimnlabgrs_dv",
                         ## education variables
                         "f_hiqual_dv",
                         ## health variables
                         "f_health","f_aidhh",
                         ## smoking variables
                         "f_smoker", "f_ncigs",
                         ## weight
                         "f_indpxub_lw"),

                       c("pidp","pid","hidp","person_number","psu","strata",
                         ## demographic
                         "sex","age","birth_year","region","urban",
                         ## economic status
                         "econ_stat",
                         ## work variables
                         "grss_pay_usual","grss_pay_last","hours","grss_lab_inc",
                         ## education variables
                         "highest_qual",
                         ## health variables
                         "lt_sick","caring",
                         ## smoking variables
                         "smoker", "ncigs",
                         ## weight
                         "weight"))

  data$wave <- "UKHLS Wave 6"
  data$wave_no <- 24
  data$bhps_sample <- ifelse(!is.na(data$pid),TRUE,FALSE)
  data$dataset <- "UKHLS"
  data$id <- ifelse(data$bhps_sample==FALSE,data$pidp,data$pid)

  ######## Add in cross-wave data

  data.xwave <- data.table::fread(
    paste0(root[1], path, "ukhls_wx/xwavedat.tab"),
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(data.xwave, names(data.xwave), tolower(names(data.xwave)))

  xwave_vars  <- colnames(data.xwave[ , c(1,34,17,18)])

  data.xwave <- data.xwave[ , xwave_vars, with = F]
  data.table::setnames(data.xwave,
                       # old names
                       c("pidp","racel_dv","dcsedfl_dv","dcsedw_dv"),
                       # new names
                       c("pidp","ethnicity_raw","deceased","deceased_when"))

  ####### Combine - keep all observations in the main data and drop excess xwave observations

  data_merged <- merge(x = data,
                       y = data.xwave,
                       by="pidp",
                       all.x=TRUE,
                       all.y=FALSE)

  return(data_merged[])
}
