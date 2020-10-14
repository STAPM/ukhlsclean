
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
ukhls_read_wave1 <- function(
  root = c("C:/"),
  path = "Users/User/Documents/Datasets/UKHLS/tab/"
) {


print("Reading UKHLS Wave 1")
  data <- data.table::fread(
    paste0(root[1], path, "ukhls_w1/a_indresp.tab"),
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-10", "-90", "-90.0", "N/A")
  )

  data.table::setnames(data, names(data), tolower(names(data)))

  id_vars          <- colnames(data[, c(1,2,3,6,7,17,16,15)])
  demographic_vars <- colnames(data[, c(12,13,14,1303,1304,25)])
  education_vars   <- colnames(data[, c(1339)])
  health_vars      <- colnames(data[, c(169,235,147,1061,1064)])
  econ_stat_vars   <- colnames(data[, c(23)])
  work_vars        <- colnames(data[, c(1266,1267,272,1256)])
  weight_vars      <- colnames(data[, c(1398)])


  names <- c(id_vars,demographic_vars,econ_stat_vars,work_vars,education_vars,health_vars,weight_vars)
  names <- tolower(names)

  data <- data[ , names, with = F]

  data.table::setnames(data,

                       c("pidp","a_hidp","a_pno","a_psu","a_strata","a_istrtdaty","a_istrtdatm","a_istrtdatd",
                         ## demographic
                         "a_sex","a_dvage","a_birthy","a_gor_dv","a_urban_dv","a_mlstat",
                         ## economic status
                         "a_jbstat",
                         ## work variables
                         "a_paygu_dv","a_payg_dv","a_jbhrs","a_fimnlabgrs_dv",
                         ## education variables
                         "a_hiqual_dv",
                         ## health variables
                         "a_health","a_aidhh","a_sf1","a_sclfsat1","a_sclfsato",
                         ## weight
                         "a_indinus_xw"),

                       c("pidp","hidp","person_number","psu","strata","year","month","day",
                         ## demographic
                         "sex","age","birth_year","region","urban","mlstat",
                         ## economic status
                         "econ_stat",
                         ## work variables
                         "grss_pay_usual","grss_pay_last","hours","grss_lab_inc",
                         ## education variables
                         "highest_qual",
                         ## health variables
                         "lt_sick","caring","gen_health","health_satisf","life_satisf",
                         ## weight
                         "weight_xw"))

  data$wave <- "UKHLS Wave 1"
  data$wave_no <- 1
  data$bhps_sample <- FALSE
  data$dataset <- "UKHLS"
  data$id <- data$pidp

  ######## ADD IN HOUSEHOLD DATA

  data.hhold <- data.table::fread(
    paste0(root[1], path, "ukhls_w1/a_hhresp.tab"),
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(data.hhold, names(data.hhold), tolower(names(data.hhold)))

  hhold_vars <- colnames(data.hhold[, c(1,225,173,174,178,207,213,214,215,216)])

  data.hhold <- data.hhold[ , hhold_vars, with = F]
  data.table::setnames(data.hhold,
                       # old names
                       c("a_hidp","a_tenure_dv","a_numadult","a_numchild","a_hhsize","a_hhtype_dv",
                         "a_nch02_dv","a_nch34_dv","a_nch511_dv","a_nch1215_dv"),
                       # new names
                       c("hidp","hh_tenure","hh_numadult","hh_numchild","hh_size","hh_type",
                         "hh_numchild02","hh_numchild34","hh_numchild511","hh_numchild1215"))

  hhold_merged <- merge(x = data,
                        y = data.hhold,
                        by="hidp",
                        all.x=TRUE,
                        all.y=FALSE)

  ######## ADD IN CROSS-WAVE DATA

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

  ## Combine - keep all observations in the main data and drop excess xwave observations

  data_merged <- merge(x = hhold_merged,
                       y = data.xwave,
                                   by="pidp",
                                   all.x=TRUE,
                                   all.y=FALSE)


  return(data_merged[])
}
