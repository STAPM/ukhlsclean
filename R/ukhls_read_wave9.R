#' Read Understanding Society Wave 9
#'
#' Reads and does basic cleaning on the UKHLS ninth wave.
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
ukhls_read_wave9 <- function(
  root = c("C:/"),
  file = "Users/cm1djm/Documents/Datasets/UKHLS/tab/",
  full = TRUE
) {

  cat(crayon::magenta("\tReading UKHLS Wave 9 datasets"))

  cat(crayon::red("\tIndividual..."))

  path <- here::here(paste0(root, file))

  data <- data.table::fread(
    paste0(path, "/i_indresp.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-10", "-90", "-90.0", "N/A")
  )
  if (full == TRUE) {
    # retain full interviews only
    data <- data[i_ivfio==1,]
  }

  data.table::setnames(data, names(data), tolower(names(data)))

  id_vars          <- Hmisc::Cs(pidp,pid,i_hidp,i_pno,i_psu,i_strata,i_istrtdaty,i_istrtdatm,i_istrtdatd)
  demographic_vars <- Hmisc::Cs(i_sex,i_dvage,i_birthy,i_gor_dv,i_urban_dv,i_mlstat, i_marstat)
  econ_stat_vars   <- Hmisc::Cs(i_jbstat,i_jbhas,i_jboff,i_jboffy)
  work_vars        <- Hmisc::Cs(i_paygu_dv, i_payg_dv, i_jbhrs, i_fimnlabgrs_dv, i_seearngrs_dv, i_jbsic07_cc)
  education_vars   <- Hmisc::Cs(i_hiqual_dv)
  health_vars      <- Hmisc::Cs(i_health,i_aidhh,i_sclfsat1,i_sclfsato,i_sf12pcs_dv,i_sf12mcs_dv,
                                i_scsf1,i_scsf2a,i_scsf2b,i_scsf3a,i_scsf3b,i_scsf4a,i_scsf4b,i_scsf5,i_scsf6a,i_scsf6b,i_scsf6c,i_scsf7)
  preg_vars        <- Hmisc::Cs(i_pregout1,i_pregout2,i_pregout3)
  smoke_vars       <- Hmisc::Cs(i_smoker,i_ncigs)
  alc_vars         <- Hmisc::Cs(i_auditc1,i_auditc2,i_auditc3,i_auditc4,i_auditc5)
  weight_vars      <- Hmisc::Cs(i_indinus_lw,i_indinui_xw)


  names <- c(id_vars,demographic_vars,econ_stat_vars,work_vars,education_vars,health_vars,preg_vars,smoke_vars,alc_vars,weight_vars)
  names <- tolower(names)

  data <- data[ , names, with = F]

  data.table::setnames(data,

                       c("pidp","pid","i_hidp","i_pno","i_psu","i_strata","i_istrtdaty","i_istrtdatm","i_istrtdatd",
                         ## demographic
                         "i_sex","i_dvage","i_birthy","i_gor_dv","i_urban_dv","i_mlstat","i_marstat",
                         ## economic status
                         "i_jbstat","i_jbhas","i_jboff","i_jboffy",
                         ## work variables
                         "i_paygu_dv","i_payg_dv","i_jbhrs","i_fimnlabgrs_dv","i_seearngrs_dv","i_jbsic07_cc",
                         ## education variables
                         "i_hiqual_dv",
                         ## health variables
                         "i_health","i_aidhh","i_sclfsat1","i_sclfsato","i_sf12pcs_dv","i_sf12mcs_dv",
                         "i_scsf1","i_scsf2a","i_scsf2b","i_scsf3a","i_scsf3b","i_scsf4a","i_scsf4b","i_scsf5","i_scsf6a","i_scsf6b","i_scsf6c","i_scsf7",
                         ## pregnancy variables
                         "i_pregout1","i_pregout2","i_pregout3",
                         ## smoke variables
                         "i_smoker", "i_ncigs",
                         ## alcohol variables
                         "i_auditc1","i_auditc2","i_auditc3","i_auditc4","i_auditc5",
                         ## weight
                         "i_indinus_lw","i_indinui_xw"),

                       c("pidp","pid","hidp","person_number","psu","strata","year","month","day",
                         ## demographic
                         "sex","age","birth_year","region","urban","mlstat","marstat",
                         ## economic status
                         "econ_stat","jbhas","jboff","jboffy",
                         ## work variables
                         "grss_pay_usual","grss_pay_last","hours","grss_lab_inc","grss_semp","sic07",
                         ## education variables
                         "highest_qual",
                         ## health variables
                         "lt_sick","caring","health_satisf","life_satisf","sf12_pcs","sf12_mcs",
                         "sf1","sf2a","sf2b","sf3a","sf3b","sf4a","sf4b","sf5","sf6a","sf6b","sf6c","sf7",
                         ## pregnancy variables
                         "pregout1","pregout2","pregout3",
                         ## smoke variables
                         "smoker", "ncigs",
                         ## alcohol variables
                         "auditc1","auditc2","auditc3","auditc4","auditc5",
                         ## weight
                         "weight_lw","weight_xw"))

  data[, wave := "UKHLS Wave 9"]
  data[, wave_no := 9]
  data[, bhps_sample := ifelse(!is.na(pid),TRUE,FALSE)]
  data[, dataset := "UKHLS"]
  data[, id := ifelse(bhps_sample==FALSE, pidp, pid)]

  ########################################
  ######## ADD IN HOUSEHOLD DATA #########

  cat(crayon::red("\tHousehold..."))

  data.hhold <- data.table::fread(
    paste0(path, "/i_hhresp.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(data.hhold, names(data.hhold), tolower(names(data.hhold)))

  hhold_vars          <- Hmisc::Cs(i_hidp, i_tenure_dv, i_nkids_dv, i_hhsize, i_hhtype_dv,
                                   i_nch02_dv, i_nch34_dv, i_nch511_dv, i_nch1215_dv)

  data.hhold <- data.hhold[ , hhold_vars, with = F]
  data.table::setnames(data.hhold,
                       # old names
                       c("i_hidp","i_tenure_dv","i_nkids_dv","i_hhsize","i_hhtype_dv",
                         "i_nch02_dv","i_nch34_dv","i_nch511_dv","i_nch1215_dv"),
                       # new names
                       c("hidp","hh_tenure","hh_numchild","hh_size","hh_type",
                         "hh_numchild02","hh_numchild34","hh_numchild511","hh_numchild1215"))

  hhold_merged <- merge(x = data,
                        y = data.hhold,
                        by = "hidp",
                        all.x=TRUE,
                        all.y=FALSE)

  #########################################
  ######## ADD IN CROSS-WAVE DATA #########

  cat(crayon::red("\tCross-Wave..."))

  data.xwave <- data.table::fread(
    paste0(path, "/xwavedat.tab"),
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

  ####### Combine - keep all observations in the main data and drop excess xwave observations

  data_merged <- merge(x = hhold_merged,
                       y = data.xwave,
                       by="pidp",
                       all.x=TRUE,
                       all.y=FALSE)

  cat(crayon::magenta("\tdone\n"))

  return(data_merged[])
}
