#' Read Understanding Society Wave 7
#'
#' Reads and does basic cleaning on the UKHLS seventh wave.
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
ukhls_read_wave7 <- function(
  root = c("C:/"),
  file = "Users/cm1djm/Documents/Datasets/UKHLS/tab/",
  full = TRUE
) {

  cat(crayon::blue("\tReading UKHLS Wave 7"))

  cat(crayon::cyan("\tIndividual..."))

  path <- here::here(paste0(root, file))

  data <- data.table::fread(
    paste0(path, "/ukhls_w7/g_indresp.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-10", "-90", "-90.0", "N/A")
  )
  if (full == TRUE) {
    # retain full interviews only
    data <- data[g_ivfio==1,]
  }

  data.table::setnames(data, names(data), tolower(names(data)))

  id_vars          <- Hmisc::Cs(pidp,pid,g_hidp,g_pno,g_psu,g_strata,g_istrtdaty,g_istrtdatm,g_istrtdatd)
  demographic_vars <- Hmisc::Cs(g_sex,g_dvage,g_birthy,g_gor_dv,g_urban_dv,g_mlstat)
  econ_stat_vars   <- Hmisc::Cs(g_jbstat,g_jbhas,g_jboff,g_jboffy)
  work_vars        <- Hmisc::Cs(g_paygu_dv,g_payg_dv,g_jbhrs,g_fimnlabgrs_dv,g_seearngrs_dv)
  education_vars   <- Hmisc::Cs(g_hiqual_dv)
  health_vars      <- Hmisc::Cs(g_health,g_aidhh,g_sclfsat1,g_sclfsato,g_sf12pcs_dv,g_sf12mcs_dv,
                                g_scsf1,g_scsf2a,g_scsf2b,g_scsf3a,g_scsf3b,g_scsf4a,g_scsf4b,g_scsf5,g_scsf6a,g_scsf6b,g_scsf6c,g_scsf7)
  preg_vars        <- Hmisc::Cs(g_pregout1,g_pregout2,g_pregout3)
  smoke_vars       <- Hmisc::Cs(g_smoker,g_ncigs)
  alc_vars         <- Hmisc::Cs(g_auditc1,g_auditc2,g_auditc3,g_auditc4,g_auditc5)
  weight_vars      <- Hmisc::Cs(g_indinus_lw,g_indinub_xw)


  names <- c(id_vars,demographic_vars,econ_stat_vars,work_vars,education_vars,health_vars,preg_vars,smoke_vars,alc_vars,weight_vars)
  names <- tolower(names)

  data <- data[ , names, with = F]

  data.table::setnames(data,

                       c("pidp","pid","g_hidp","g_pno","g_psu","g_strata","g_istrtdaty","g_istrtdatm","g_istrtdatd",
                         ## demographic
                         "g_sex","g_dvage","g_birthy","g_gor_dv","g_urban_dv","g_mlstat",
                         ## economic stauts
                         "g_jbstat","g_jbhas","g_jboff","g_jboffy",
                         ## work variables
                         "g_paygu_dv","g_payg_dv","g_jbhrs","g_fimnlabgrs_dv","g_seearngrs_dv",
                         ## education variables
                         "g_hiqual_dv",
                         ## health variables
                         "g_health","g_aidhh","g_sclfsat1","g_sclfsato","g_sf12pcs_dv","g_sf12mcs_dv",
                         "g_scsf1","g_scsf2a","g_scsf2b","g_scsf3a","g_scsf3b","g_scsf4a","g_scsf4b","g_scsf5","g_scsf6a","g_scsf6b","g_scsf6c","g_scsf7",
                         ## pregnancy variables
                         "g_pregout1","g_pregout2","g_pregout3",
                         ## smoking variables
                         "g_smoker", "g_ncigs",
                         ## alcohol variables
                         "g_auditc1","g_auditc2","g_auditc3","g_auditc4","g_auditc5",
                         ## weight
                         "g_indinus_lw","g_indinub_xw"),

                       c("pidp","pid","hidp","person_number","psu","strata","year","month","day",
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
                         ## pregnancy variables
                         "pregout1","pregout2","pregout3",
                         ## smoking variables
                         "smoker", "ncigs",
                         ## alcohol variables
                         "auditc1","auditc2","auditc3","auditc4","auditc5",
                         ## weight
                         "weight_lw","weight_xw"))

  data[, wave := "UKHLS Wave 7"]
  data[, wave_no := 7]
  data[, bhps_sample := ifelse(!is.na(pid),TRUE,FALSE)]
  data[, dataset := "UKHLS"]
  data[, id := ifelse(bhps_sample==FALSE, pidp, pid)]

  ########################################
  ######## ADD IN HOUSEHOLD DATA #########

  cat(crayon::cyan("\tHousehold..."))

  data.hhold <- data.table::fread(
    paste0(path, "/ukhls_w7/g_hhresp.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(data.hhold, names(data.hhold), tolower(names(data.hhold)))

  hhold_vars <- colnames(data.hhold[, c(1,340,327,290,322,328,329,330,331)])

  data.hhold <- data.hhold[ , hhold_vars, with = F]
  data.table::setnames(data.hhold,
                       # old names
                       c("g_hidp","g_tenure_dv","g_nkids_dv","g_hhsize","g_hhtype_dv",
                         "g_nch02_dv","g_nch34_dv","g_nch511_dv","g_nch1215_dv"),
                       # new names
                       c("hidp","hh_tenure","hh_numchild","hh_size","hh_type",
                         "hh_numchild02","hh_numchild34","hh_numchild511","hh_numchild1215"))

  hhold_merged <- merge(x = data,
                        y = data.hhold,
                        by="hidp",
                        all.x=TRUE,
                        all.y=FALSE)

  #########################################
  ######## ADD IN CROSS-WAVE DATA #########

  cat(crayon::cyan("\tCross-Wave..."))

  data.xwave <- data.table::fread(
    paste0(path, "/ukhls_wx/xwavedat.tab"),
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

  cat(crayon::white("\tdone\n"))

  return(data_merged)
}
