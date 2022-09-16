#' Read Understanding Society Wave 5
#'
#' Reads and does basic cleaning on the UKHLS fifth wave.
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
ukhls_read_wave5 <- function(
  root = c("C:/"),
  file = "Users/cm1djm/Documents/Datasets/UKHLS/tab/",
  full = TRUE
) {

  cat(crayon::magenta("\tReading UKHLS Wave 5 datasets"))

  cat(crayon::red("\tIndividual..."))

  path <- here::here(paste0(root, file))

  data <- data.table::fread(
    paste0(path, "/ukhls_w5/e_indresp.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-10", "-90", "-90.0", "N/A")
  )
  if (full == TRUE) {
    # retain full interviews only
    data <- data[e_ivfio==1,]
  }

  data.table::setnames(data, names(data), tolower(names(data)))

  id_vars          <- Hmisc::Cs(pidp,pid,e_hidp,e_pno,e_psu,e_strata,e_istrtdaty,e_istrtdatm,e_istrtdatd)
  demographic_vars <- Hmisc::Cs(e_sex,e_dvage,e_birthy,e_gor_dv,e_urban_dv,e_mlstat)
  econ_stat_vars   <- Hmisc::Cs(e_jbstat,e_jbhas,e_jboff,e_jboffy)
  work_vars        <- Hmisc::Cs(e_paygu_dv,e_payg_dv,e_jbhrs,e_fimnlabgrs_dv,e_seearngrs_dv)
  education_vars   <- Hmisc::Cs(e_hiqual_dv)
  health_vars      <- Hmisc::Cs(e_health,e_aidhh,e_sclfsat1,e_sclfsato,e_sf12pcs_dv,e_sf12mcs_dv,
                                e_scsf1,e_scsf2a,e_scsf2b,e_scsf3a,e_scsf3b,e_scsf4a,e_scsf4b,e_scsf5,e_scsf6a,e_scsf6b,e_scsf6c,e_scsf7)
  preg_vars        <- Hmisc::Cs(e_pregout1,e_pregout2,e_pregout3)
  smoke_vars       <- Hmisc::Cs(e_smever,e_smnow,e_ncigs,e_smcigs,e_smncigs,e_aglquit,e_smagbg)
  alc_vars         <- Hmisc::Cs(e_sceverdrnk,e_scfalcdrnk)
  weight_vars      <- Hmisc::Cs(e_indinus_lw,e_indinub_xw)


  names <- c(id_vars,demographic_vars,econ_stat_vars,work_vars,education_vars,health_vars,preg_vars,smoke_vars,alc_vars,weight_vars)
  names <- tolower(names)

  data <- data[ , names, with = F]

  data.table::setnames(data,

                       c("pidp","pid","e_hidp","e_pno","e_psu","e_strata","e_istrtdaty","e_istrtdatm","e_istrtdatd",
                         ## demographic
                         "e_sex","e_dvage","e_birthy","e_gor_dv","e_urban_dv","e_mlstat",
                         ## economic status
                         "e_jbstat","e_jbhas","e_jboff","e_jboffy",
                         ## work variables
                         "e_paygu_dv","e_payg_dv","e_jbhrs","e_fimnlabgrs_dv","e_seearngrs_dv",
                         ## education variables
                         "e_hiqual_dv",
                         ## health variables
                         "e_health","e_aidhh","e_sclfsat1","e_sclfsato","e_sf12pcs_dv","e_sf12mcs_dv",
                         "e_scsf1","e_scsf2a","e_scsf2b","e_scsf3a","e_scsf3b","e_scsf4a","e_scsf4b","e_scsf5","e_scsf6a","e_scsf6b","e_scsf6c","e_scsf7",
                         ## pregnancy variables
                         "e_pregout1","e_pregout2","e_pregout3",
                         ## smoking variables
                         "e_smever","e_smnow","e_ncigs","e_smcigs","e_smncigs","e_aglquit","e_smagbg",
                         ## alcohol variables
                         "e_sceverdrnk","e_scfalcdrnk",
                         ## weight
                         "e_indinus_lw","e_indinub_xw"),

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
                         "smever","smnow","ncigs","smcigs","smncigs","aglquit","smagbg",
                         ## alcohol variables
                         "sceverdrnk","scfalcdrnk",
                         ## weight
                         "weight_lw","weight_xw"))

  data[, wave := "UKHLS Wave 5"]
  data[, wave_no := 5]
  data[, bhps_sample := ifelse(!is.na(pid),TRUE,FALSE)]
  data[, dataset := "UKHLS"]
  data[, id := ifelse(bhps_sample==FALSE, pidp, pid)]

  ########################################
  ######## ADD IN HOUSEHOLD DATA #########

  cat(crayon::red("\tHousehold..."))

  data.hhold <- data.table::fread(
    paste0(path, "/ukhls_w5/e_hhresp.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(data.hhold, names(data.hhold), tolower(names(data.hhold)))

  hhold_vars <- colnames(data.hhold[, c(1,287,274,236,269,275,276,277,278)])

  data.hhold <- data.hhold[ , hhold_vars, with = F]
  data.table::setnames(data.hhold,
                       # old names
                       c("e_hidp","e_tenure_dv","e_nkids_dv","e_hhsize","e_hhtype_dv",
                         "e_nch02_dv","e_nch34_dv","e_nch511_dv","e_nch1215_dv"),
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

  cat(crayon::red("\tCross-Wave..."))

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

  cat(crayon::magenta("\tdone\n"))

  return(data_merged)
}
