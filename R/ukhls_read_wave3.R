#' Read Understanding Society Wave 3
#'
#' Reads and does basic cleaning on the UKHLS third wave.
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
ukhls_read_wave3 <- function(
  root = c("C:/"),
  path = "Users/cm1djm/Documents/Datasets/UKHLS/tab/",
  full = TRUE
) {
  cat(crayon::blue("\tReading UKHLS Wave 3"))

  cat(crayon::cyan("\tIndividual..."))

  file <- here::here(paste0(root, path))

  data <- data.table::fread(
    paste0(file, "/ukhls_w3/c_indresp.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-10", "-90", "-90.0", "N/A")
  )
  if (full == TRUE) {
    # retain full interviews only
    data <- data[c_ivfio==1,]
  }

  data.table::setnames(data, names(data), tolower(names(data)))

  id_vars          <- Hmisc::Cs(pidp,pid,c_hidp,c_pno,c_psu,c_strata,c_istrtdaty,c_istrtdatm,c_istrtdatd)
  demographic_vars <- Hmisc::Cs(c_sex,c_dvage,c_birthy,c_gor_dv,c_urban_dv,c_mlstat)
  econ_stat_vars   <- Hmisc::Cs(c_jbstat,c_jbhas,c_jboff,c_jboffy)
  work_vars        <- Hmisc::Cs(c_paygu_dv,c_payg_dv,c_jbhrs,c_fimnlabgrs_dv,c_seearngrs_dv)
  education_vars   <- Hmisc::Cs(c_hiqual_dv)
  health_vars      <- Hmisc::Cs(c_health,c_aidhh,c_sclfsat1,c_sclfsato,c_sf12pcs_dv,c_sf12mcs_dv,
                                c_scsf1,c_scsf2a,c_scsf2b,c_scsf3a,c_scsf3b,c_scsf4a,c_scsf4b,c_scsf5,c_scsf6a,c_scsf6b,c_scsf6c,c_scsf7)
  preg_vars        <- Hmisc::Cs(c_pregout1,c_pregout2,c_pregout3)
  alc_vars         <- Hmisc::Cs(c_dklm,c_drnk4w,c_evralc,c_fivealcdr)
  weight_vars      <- Hmisc::Cs(c_indinus_lw,c_indinub_xw)


  names <- c(id_vars,demographic_vars,econ_stat_vars,work_vars,education_vars,health_vars,preg_vars,alc_vars,weight_vars)
  names <- tolower(names)

  data <- data[ , names, with = F]

  data.table::setnames(data,

                       c("pidp","pid","c_hidp","c_pno","c_psu","c_strata","c_istrtdaty","c_istrtdatm","c_istrtdatd",
                         ## demographic
                         "c_sex","c_dvage","c_birthy","c_gor_dv","c_urban_dv","c_mlstat",
                         ## economic stauts
                         "c_jbstat","c_jbhas","c_jboff","c_jboffy",
                         ## work variables
                         "c_paygu_dv","c_payg_dv","c_jbhrs","c_fimnlabgrs_dv","c_seearngrs_dv",
                         ## education variables
                         "c_hiqual_dv",
                         ## health variables
                         "c_health","c_aidhh","c_sclfsat1","c_sclfsato","c_sf12pcs_dv","c_sf12mcs_dv",
                         "c_scsf1","c_scsf2a","c_scsf2b","c_scsf3a","c_scsf3b","c_scsf4a","c_scsf4b","c_scsf5","c_scsf6a","c_scsf6b","c_scsf6c","c_scsf7",
                         ## pregnancy variables
                         "c_pregout1","c_pregout2","c_pregout3",
                         ## alcohol variables
                         "c_dklm","c_drnk4w","c_evralc","c_fivealcdr",
                         ## weight
                         "c_indinus_lw","c_indinub_xw"),

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
                         ## alcohol variables
                         "dklm","drnk4w","evralc","fivealcdr",
                         ## weight
                         "weight_lw","weight_xw"))

  data[, wave := "UKHLS Wave 3"]
  data[, wave_no := 3]
  data[, bhps_sample := ifelse(!is.na(pid),TRUE,FALSE)]
  data[, dataset := "UKHLS"]
  data[, id := ifelse(bhps_sample==FALSE, pidp, pid)]

  ######## ADD IN HOUSEHOLD DATA

  cat(crayon::cyan("\tHousehold..."))

  data.hhold <- data.table::fread(
    paste0(root[1], path, "ukhls_w3/c_hhresp.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(data.hhold, names(data.hhold), tolower(names(data.hhold)))

  hhold_vars <- colnames(data.hhold[, c(1,222,11,12,10,204,210,211,212,213)])

  data.hhold <- data.hhold[ , hhold_vars, with = F]
  data.table::setnames(data.hhold,
                       # old names
                       c("c_hidp","c_tenure_dv","c_numadult","c_nkids015","c_hhsize","c_hhtype_dv",
                         "c_nch02_dv","c_nch34_dv","c_nch511_dv","c_nch1215_dv"),
                       # new names
                       c("hidp","hh_tenure","hh_numadult","hh_numchild","hh_size","hh_type",
                         "hh_numchild02","hh_numchild34","hh_numchild511","hh_numchild1215"))

  hhold_merged <- merge(x = data,
                        y = data.hhold,
                        by="hidp",
                        all.x=TRUE,
                        all.y=FALSE)

  ######## Add in cross-wave data

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

  ####### Combine - keep all observations in the main data and drop excess xwave observations

  data_merged <- merge(x = hhold_merged,
                       y = data.xwave,
                       by="pidp",
                       all.x=TRUE,
                       all.y=FALSE)

  cat(crayon::white("\tdone\n"))

  return(data_merged[])
}
