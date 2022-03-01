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
ukhls_read_wave6 <- function(
  root = c("C:/"),
  path = "Users/cm1djm/Documents/Datasets/UKHLS/tab/",
  full = TRUE
) {

  cat(crayon::blue("\tReading UKHLS Wave 6"))

  cat(crayon::cyan("\tIndividual..."))

  file <- here::here(paste0(root, path))

  data <- data.table::fread(
    paste0(file, "/ukhls_w6/f_indresp.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-10", "-90", "-90.0", "N/A")
  )
  if (full == TRUE) {
    # retain full interviews only
    data <- data[f_ivfio==1,]
  }

  data.table::setnames(data, names(data), tolower(names(data)))

  id_vars          <- Hmisc::Cs(pidp,pid,f_hidp,f_pno,f_psu,f_strata,f_istrtdaty,f_istrtdatm,f_istrtdatd)
  demographic_vars <- Hmisc::Cs(f_sex,f_dvage,f_birthy,f_gor_dv,f_urban_dv,f_mlstat)
  econ_stat_vars   <- Hmisc::Cs(f_jbstat,f_jbhas,f_jboff,f_jboffy)
  work_vars        <- Hmisc::Cs(f_paygu_dv,f_payg_dv,f_jbhrs,f_fimnlabgrs_dv,f_seearngrs_dv)
  education_vars   <- Hmisc::Cs(f_hiqual_dv)
  health_vars      <- Hmisc::Cs(f_health,f_aidhh,f_sclfsat1,f_sclfsato,f_sf12pcs_dv,f_sf12mcs_dv,
                                f_scsf1,f_scsf2a,f_scsf2b,f_scsf3a,f_scsf3b,f_scsf4a,f_scsf4b,f_scsf5,f_scsf6a,f_scsf6b,f_scsf6c,f_scsf7)
  preg_vars        <- Hmisc::Cs(f_pregout1,f_pregout2,f_pregout3,f_pregout4,f_pregout5)
  smoke_vars       <- Hmisc::Cs(f_smoker, f_ncigs)
  alc_vars         <- Hmisc::Cs(f_dklm,f_drnk4w,f_evralc,f_fivealcdr)
  weight_vars      <- Hmisc::Cs(f_indinus_lw,f_indinub_xw)


  names <- c(id_vars,demographic_vars,econ_stat_vars,work_vars,education_vars,health_vars,preg_vars,smoke_vars,alc_vars,weight_vars)
  names <- tolower(names)

  data <- data[ , names, with = F]

  data.table::setnames(data,

                       c("pidp","pid","f_hidp","f_pno","f_psu","f_strata","f_istrtdaty","f_istrtdatm","f_istrtdatd",
                         ## demographic
                         "f_sex","f_dvage","f_birthy","f_gor_dv","f_urban_dv","f_mlstat",
                         ## economic stauts
                         "f_jbstat","f_jbhas","f_jboff","f_jboffy",
                         ## work variables
                         "f_paygu_dv","f_payg_dv","f_jbhrs","f_fimnlabgrs_dv","f_seearngrs_dv",
                         ## education variables
                         "f_hiqual_dv",
                         ## health variables
                         "f_health","f_aidhh","f_sclfsat1","f_sclfsato","f_sf12pcs_dv","f_sf12mcs_dv",
                         "f_scsf1","f_scsf2a","f_scsf2b","f_scsf3a","f_scsf3b","f_scsf4a","f_scsf4b","f_scsf5","f_scsf6a","f_scsf6b","f_scsf6c","f_scsf7",
                         ## pregnancy variables
                         "f_pregout1","f_pregout2","f_pregout3","f_pregout4","f_pregout5",
                         ## smoking variables
                         "f_smoker", "f_ncigs",
                         ## alcohol variables
                         "f_dklm","f_drnk4w","f_evralc","f_fivealcdr",
                         ## weight
                         "f_indinus_lw","f_indinub_xw"),

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
                         "pregout1","pregout2","pregout3","pregout4","pregout5",
                         ## smoking variables
                         "smoker", "ncigs",
                         ## alcohol variables
                         "dklm","drnk4w","evralc","fivealcdr",
                         ## weight
                         "weight_lw","weight_xw"))

  data[, wave := "UKHLS Wave 6"]
  data[, wave_no := 6]
  data[, bhps_sample := ifelse(!is.na(pid),TRUE,FALSE)]
  data[, dataset := "UKHLS"]
  data[, id := ifelse(bhps_sample==FALSE, pidp, pid)]

  ######## ADD IN HOUSEHOLD DATA

  cat(crayon::cyan("\tHousehold..."))

  data.hhold <- data.table::fread(
    paste0(root[1], path, "ukhls_w6/f_hhresp.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(data.hhold, names(data.hhold), tolower(names(data.hhold)))

  hhold_vars <- colnames(data.hhold[, c(1,445,432,395,427,433,434,435,436)])

  data.hhold <- data.hhold[ , hhold_vars, with = F]
  data.table::setnames(data.hhold,
                       # old names
                       c("f_hidp","f_tenure_dv","f_nkids_dv","f_hhsize","f_hhtype_dv",
                         "f_nch02_dv","f_nch34_dv","f_nch511_dv","f_nch1215_dv"),
                       # new names
                       c("hidp","hh_tenure","hh_numchild","hh_size","hh_type",
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
