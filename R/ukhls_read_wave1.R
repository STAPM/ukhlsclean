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
#' @param full Logical - TRUE if restricting the sample to full interviews only (excluding proxies)
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
  path = "Users/cm1djm/Documents/Datasets/UKHLS/tab/",
  full = TRUE
) {

  cat(crayon::blue("\tReading UKHLS Wave 1"))

  cat(crayon::cyan("\tIndividual..."))

  data <- data.table::fread(
    paste0(root[1], path, "ukhls_w1/a_indresp.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-10", "-90", "-90.0", "N/A")
  )
  if (full == TRUE) {
    # retain full interviews only
    data <- data[a_ivfio==1,]
  }

  data.table::setnames(data, names(data), tolower(names(data)))

  id_vars          <- Hmisc::Cs(pidp,a_hidp,a_pno,a_psu,a_strata,a_istrtdaty,a_istrtdatm,a_istrtdatd)
  demographic_vars <- Hmisc::Cs(a_sex,a_dvage,a_birthy,a_gor_dv,a_urban_dv,a_mlstat)
  econ_stat_vars   <- Hmisc::Cs(a_jbstat,a_jbhas,a_jboff,a_jboffy)
  work_vars        <- Hmisc::Cs(a_paygu_dv,a_payg_dv,a_jbhrs,a_fimnlabgrs_dv,a_seearngrs_dv)
  education_vars   <- Hmisc::Cs(a_hiqual_dv)
  health_vars      <- Hmisc::Cs(a_health,a_aidhh,a_sclfsat1,a_sclfsato,a_sf12pcs_dv,a_sf12mcs_dv,
                                a_sf1,a_sf2a,a_sf2b,a_sf3a,a_sf3b,a_sf4a,a_sf4b,a_sf5,a_sf6a,a_sf6b,a_sf6c,a_sf7)
  weight_vars      <- Hmisc::Cs(a_indinus_xw)


  names <- c(id_vars,demographic_vars,econ_stat_vars,work_vars,education_vars,health_vars,weight_vars)
  names <- tolower(names)

  data <- data[ , names, with = F]

  data.table::setnames(data,

                       c("pidp","a_hidp","a_pno","a_psu","a_strata","a_istrtdaty","a_istrtdatm","a_istrtdatd",
                         ## demographic
                         "a_sex","a_dvage","a_birthy","a_gor_dv","a_urban_dv","a_mlstat",
                         ## economic status
                         "a_jbstat","a_jbhas","a_jboff","a_jboffy",
                         ## work variables
                         "a_paygu_dv","a_payg_dv","a_jbhrs","a_fimnlabgrs_dv","a_seearngrs_dv",
                         ## education variables
                         "a_hiqual_dv",
                         ## health variables
                         "a_health","a_aidhh","a_sclfsat1","a_sclfsato","a_sf12pcs_dv","a_sf12mcs_dv",
                         "a_sf1","a_sf2a","a_sf2b","a_sf3a","a_sf3b","a_sf4a","a_sf4b","a_sf5","a_sf6a","a_sf6b","a_sf6c","a_sf7",
                         ## weight
                         "a_indinus_xw"),

                       c("pidp","hidp","person_number","psu","strata","year","month","day",
                         ## demographic
                         "sex","age","birth_year","region","urban","mlstat",
                         ## economic status
                         "econ_stat","jbhas","jboff","jboffy",
                         ## work variables
                         "grss_pay_usual","grss_pay_last","hours","grss_lab_inc","grss_semp",
                         ## education variables
                         "highest_qual",
                         ## health variables
                         "lt_sick","caring","health_satisf","life_satisf","sf12_pcs","sf12_mcs",
                         "sf1","sf2a","sf2b","sf3a","sf3b","sf4a","sf4b","sf5","sf6a","sf6b","sf6c","sf7",
                         ## weight
                         "weight_xw"))

  data[,wave_no := 1]
  data[,bhps_sample := FALSE]
  data[,id := data$pidp]

  ######## ADD IN HOUSEHOLD DATA

  cat(crayon::cyan("\tHousehold..."))

  data.hhold <- data.table::fread(
    paste0(root[1], path, "ukhls_w1/a_hhresp.tab"),
    showProgress = FALSE,
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

  cat(crayon::cyan("\tCross-Wave..."))

  data.xwave <- data.table::fread(
    paste0(root[1], path, "ukhls_wx/xwavedat.tab"),
    showProgress = FALSE,
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

  cat(crayon::white("\tdone\n"))

  return(data_merged[])
}
