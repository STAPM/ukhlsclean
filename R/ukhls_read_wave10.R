#' Read Understanding Society Wave 10
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
ukhls_read_wave10 <- function(
  root = c("C:/"),
  path = "Users/cm1djm/Documents/Datasets/UKHLS/tab/",
  full = TRUE
) {


  print("Reading UKHLS Wave 9")
  data <- data.table::fread(
    paste0(root[1], path, "ukhls_w10/j_indresp.tab"),
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-10", "-90", "-90.0", "N/A")
  )
  if (full == TRUE) {
    # retain full interviews only
    data <- data[i_ivfio==1,]
  }

  data.table::setnames(data, names(data), tolower(names(data)))

  id_vars          <- Hmisc::Cs(pidp,pid,j_hidp,j_pno,j_psu,j_strata,j_istrtdaty,j_istrtdatm,j_istrtdatd)
  demographic_vars <- Hmisc::Cs(j_sex,j_dvage,j_birthy,j_gor_dv,j_urban_dv,j_mlstat)
  econ_stat_vars   <- Hmisc::Cs(j_jbstat,j_jbhas,j_jboff,j_jboffy)
  work_vars        <- Hmisc::Cs(j_paygu_dv,j_payg_dv,j_jbhrs,j_fimnlabgrs_dv,j_seearngrs_dv)
  education_vars   <- Hmisc::Cs(j_hiqual_dv)
  health_vars      <- Hmisc::Cs(j_health,i_aidhh,j_sclfsat1,j_sclfsato,j_sf12pcs_dv,j_sf12mcs_dv,
                                j_scsf1,j_scsf2a,j_scsf2b,j_scsf3a,j_scsf3b,j_scsf4a,j_scsf4b,j_scsf5,j_scsf6a,j_scsf6b,j_scsf6c,j_scsf7)
  preg_vars        <- Hmisc::Cs(j_pregout1,j_pregout2,j_pregout3)
  smoke_vars       <- Hmisc::Cs(j_smoker,j_ncigs)
  alc_vars         <- Hmisc::Cs(j_auditc1,j_auditc2,j_auditc3,j_auditc4,j_auditc5)
  weight_vars      <- Hmisc::Cs(j_indinus_lw,j_indinub_xw)


  names <- c(id_vars,demographic_vars,econ_stat_vars,work_vars,education_vars,health_vars,preg_vars,smoke_vars,alc_vars,weight_vars)
  names <- tolower(names)

  data <- data[ , names, with = F]

  data.table::setnames(data,

                       c("pidp","pid","j_hidp","j_pno","j_psu","j_strata","j_istrtdaty","j_istrtdatm","j_istrtdatd",
                         ## demographic
                         "j_sex","j_dvage","j_birthy","j_gor_dv","j_urban_dv","j_mlstat",
                         ## economic status
                         "j_jbstat","j_jbhas","j_jboff","j_jboffy",
                         ## work variables
                         "j_paygu_dv","j_payg_dv","j_jbhrs","j_fimnlabgrs_dv","j_seearngrs_dv",
                         ## education variables
                         "j_hiqual_dv",
                         ## health variables
                         "j_health","j_aidhh","j_sclfsat1","j_sclfsato","j_sf12pcs_dv","j_sf12mcs_dv",
                         "j_scsf1","j_scsf2a","j_scsf2b","j_scsf3a","j_scsf3b","j_scsf4a","j_scsf4b","j_scsf5","j_scsf6a","j_scsf6b","j_scsf6c","j_scsf7",
                         ## pregnancy variables
                         "j_pregout1","j_pregout2","j_pregout3",
                         ## smoke variables
                         "j_smoker", "j_ncigs",
                         ## alcohol variables
                         "j_auditc1","j_auditc2","j_auditc3","j_auditc4","j_auditc5",
                         ## weight
                         "j_indinus_lw","j_indinub_xw"),

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
                         ## smoke variables
                         "smoker", "ncigs",
                         ## alcohol variables
                         "auditc1","auditc2","auditc3","auditc4","auditc5",
                         ## weight
                         "weight_lw","weight_xw"))

  data$wave <- "UKHLS Wave 10"
  data$wave_no <- 10
  data$bhps_sample <- ifelse(!is.na(data$pid),TRUE,FALSE)
  data$dataset <- "UKHLS"
  data$id <- ifelse(data$bhps_sample==FALSE,data$pidp,data$pid)

  ######## ADD IN HOUSEHOLD DATA

  data.hhold <- data.table::fread(
    paste0(root[1], path, "ukhls_w10/j_hhresp.tab"),
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(data.hhold, names(data.hhold), tolower(names(data.hhold)))

  hhold_vars <- colnames(data.hhold[, c(1,333,320,271,315,321,322,323,324)])

  data.hhold <- data.hhold[ , hhold_vars, with = F]
  data.table::setnames(data.hhold,
                       # old names
                       c("j_hidp","j_tenure_dv","j_nkids_dv","j_hhsize","j_hhtype_dv",
                         "j_nch02_dv","j_nch34_dv","j_nch511_dv","j_nch1215_dv"),
                       # new names
                       c("hidp","hh_tenure","hh_numchild","hh_size","hh_type",
                         "hh_numchild02","hh_numchild34","hh_numchild511","hh_numchild1215"))

  hhold_merged <- merge(x = data,
                        y = data.hhold,
                        by="hidp",
                        all.x=TRUE,
                        all.y=FALSE)

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

  data_merged <- merge(x = hhold_merged,
                       y = data.xwave,
                       by="pidp",
                       all.x=TRUE,
                       all.y=FALSE)

  cat(crayon::cyan("\tdone\n"))

  return(data_merged[])
}