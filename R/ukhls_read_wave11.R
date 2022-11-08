#' Read Understanding Society Wave 11
#'
#' Reads and does basic cleaning on the UKHLS eleventh wave.
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
ukhls_read_wave11 <- function(
  root = c("C:/"),
  file = "Users/cm1djm/Documents/Datasets/UKHLS/tab/",
  full = TRUE
) {

  cat(crayon::magenta("\tReading UKHLS Wave 11 datasets"))

  cat(crayon::red("\tIndividual..."))

  path <- here::here(paste0(root, file))

  data <- data.table::fread(
    paste0(path, "/k_indresp.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-10", "-90", "-90.0", "N/A")
  )
  if (full == TRUE) {
    # retain full interviews only
    data <- data[k_ivfio==1,]
  }

  data.table::setnames(data, names(data), tolower(names(data)))

  id_vars          <- Hmisc::Cs(pidp, pid, k_hidp, k_pno, k_psu, k_strata, k_istrtdaty, k_istrtdatm, k_istrtdatd)
  demographic_vars <- Hmisc::Cs(k_sex, k_dvage, k_birthy, k_gor_dv, k_urban_dv, k_mlstat, k_marstat)
  econ_stat_vars   <- Hmisc::Cs(k_jbstat, k_jbhas, k_jboff, k_jboffy)
  work_vars        <- Hmisc::Cs(k_paygu_dv, k_payg_dv, k_jbhrs, k_fimnlabgrs_dv, k_seearngrs_dv)
  education_vars   <- Hmisc::Cs(k_hiqual_dv)
  health_vars      <- Hmisc::Cs(k_health, k_aidhh, k_sclfsat1, k_sclfsato, k_sf12pcs_dv, k_sf12mcs_dv,
                                k_scsf1, k_scsf2a, k_scsf2b, k_scsf3a, k_scsf3b, k_scsf4a, k_scsf4b, k_scsf5, k_scsf6a,
                                k_scsf6b, k_scsf6c, k_scsf7)
  preg_vars        <- Hmisc::Cs(k_pregout1, k_pregout2, k_pregout3)
  smoke_vars       <- Hmisc::Cs(k_smoker, k_ncigs)
  alc_vars         <- Hmisc::Cs(k_auditc1, k_auditc2, k_auditc3, k_auditc4, k_auditc5)
  weight_vars      <- Hmisc::Cs(k_indinus_lw, k_indinui_xw)

  names <- c(id_vars, demographic_vars, econ_stat_vars, work_vars, education_vars,
             health_vars, preg_vars, smoke_vars, alc_vars, weight_vars)
  names <- tolower(names)

  data <- data[ , names, with = F]

  data.table::setnames(data,

                       c("pidp","pid","k_hidp","k_pno","k_psu","k_strata","k_istrtdaty","k_istrtdatm","k_istrtdatd",
                         ## demographic
                         "k_sex","k_dvage","k_birthy","k_gor_dv","k_urban_dv","k_mlstat","k_marstat",
                         ## economic status
                         "k_jbstat","k_jbhas","k_jboff","k_jboffy",
                         ## work variables
                         "k_paygu_dv","k_payg_dv","k_jbhrs","k_fimnlabgrs_dv","k_seearngrs_dv",
                         ## education variables
                         "k_hiqual_dv",
                         ## health variables
                         "k_health","k_aidhh","k_sclfsat1","k_sclfsato","k_sf12pcs_dv","k_sf12mcs_dv",
                         "k_scsf1","k_scsf2a","k_scsf2b","k_scsf3a","k_scsf3b","k_scsf4a","k_scsf4b","k_scsf5","k_scsf6a","k_scsf6b","k_scsf6c","k_scsf7",
                         ## pregnancy variables
                         "k_pregout1","k_pregout2","k_pregout3",
                         ## smoke variables
                         "k_smoker", "k_ncigs",
                         ## alcohol variables
                         "k_auditc1","k_auditc2","k_auditc3","k_auditc4","k_auditc5",
                         ## weight
                         "k_indinus_lw","k_indinui_xw"),

                       c("pidp","pid","hidp","person_number","psu","strata","year","month","day",
                         ## demographic
                         "sex","age","birth_year","region","urban","mlstat","marstat",
                         ## economic status
                         "econ_stat","jbhas","jboff","jboffy",
                         ## work variables
                         "grss_pay_usual","grss_pay_last","hours","grss_lab_inc","grss_semp",
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

  data[, wave := "UKHLS Wave 11"]
  data[, wave_no := 11]
  data[, bhps_sample := ifelse(!is.na(pid),TRUE,FALSE)]
  data[, dataset := "UKHLS"]
  data[, id := ifelse(bhps_sample==FALSE, pidp, pid)]

  ########################################
  ######## ADD IN HOUSEHOLD DATA #########

  cat(crayon::red("\tHousehold..."))

  data.hhold <- data.table::fread(
    paste0(path, "/k_hhresp.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(data.hhold, names(data.hhold), tolower(names(data.hhold)))

  hhold_vars <- colnames(data.hhold[, c(1,318,305,266,300,306,307,308,309)])

  data.hhold <- data.hhold[ , hhold_vars, with = F]
  data.table::setnames(data.hhold,
                       # old names
                       c("k_hidp", "k_tenure_dv", "k_nkids_dv", "k_hhsize","k_hhtype_dv",
                         "k_nch02_dv", "k_nch34_dv", "k_nch511_dv", "k_nch1215_dv"),
                       # new names
                       c("hidp", "hh_tenure", "hh_numchild", "hh_size", "hh_type",
                         "hh_numchild02", "hh_numchild34", "hh_numchild511", "hh_numchild1215"))

  hhold_merged <- merge(x = data,
                        y = data.hhold,
                        by="hidp",
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

  return(data_merged)
}
