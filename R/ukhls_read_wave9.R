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
ukhls_read_wave9 <- function(
  root = c("C:/"),
  path = "Users/cm1djm/Documents/Datasets/UKHLS/tab/",
  full = TRUE
) {


  print("Reading UKHLS Wave 9")
  data <- data.table::fread(
    paste0(root[1], path, "ukhls_w9/i_indresp.tab"),
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-10", "-90", "-90.0", "N/A")
  )
  if (full == TRUE) {
    # retain full interviews only
    data <- data[i_ivfio==1,]
  }

  data.table::setnames(data, names(data), tolower(names(data)))

  id_vars          <- colnames(data[,c(1,2,3,4,8,9,59,60,61)])
  demographic_vars <- colnames(data[,c(15,16,17,2982,2983,86)])
  econ_stat_vars   <- colnames(data[,c(84)])
  work_vars        <- colnames(data[,c(2884,2885,1646,2874,2888)])
  education_vars   <- colnames(data[,c(3020)])
  health_vars      <- colnames(data[,c(924,1226,2215,2247,2250)])
  preg_vars        <- colnames(data[,c(2642,2667,2692)])
  smoke_vars       <- colnames(data[,c(1043,1044)])
  weight_vars      <- colnames(data[,c(3103,3104)])


  names <- c(id_vars,demographic_vars,econ_stat_vars,work_vars,education_vars,health_vars,preg_vars,smoke_vars,weight_vars)
  names <- tolower(names)

  data <- data[ , names, with = F]

  data.table::setnames(data,

                       c("pidp","pid","i_hidp","i_pno","i_psu","i_strata","i_istrtdaty","i_istrtdatm","i_istrtdatd",
                         ## demographic
                         "i_sex","i_dvage","i_birthy","i_gor_dv","i_urban_dv","i_mlstat",
                         ## economic status
                         "i_jbstat",
                         ## work variables
                         "i_paygu_dv","i_payg_dv","i_jbhrs","i_fimnlabgrs_dv","i_seearngrs_dv",
                         ## education variables
                         "i_hiqual_dv",
                         ## health variables
                         "i_health","i_aidhh","i_scsf1","i_sclfsat1","i_sclfsato",
                         ## pregnancy variables
                         "i_pregout1","i_pregout2","i_pregout3",
                         ## smoke variables
                         "i_smoker", "i_ncigs",
                         ## weight
                         "i_indinus_lw","i_indinub_xw"),

                       c("pidp","pid","hidp","person_number","psu","strata","year","month","day",
                         ## demographic
                         "sex","age","birth_year","region","urban","mlstat",
                         ## economic status
                         "econ_stat",
                         ## work variables
                         "grss_pay_usual","grss_pay_last","hours","grss_lab_inc","grss_semp",
                         ## education variables
                         "highest_qual",
                         ## health variables
                         "lt_sick","caring","gen_health","health_satisf","life_satisf",
                         ## pregnancy variables
                         "pregout1","pregout2","pregout3",
                         ## smoke variables
                         "smoker", "ncigs",
                         ## weight
                         "weight_lw","weight_xw"))

  data$wave <- "UKHLS Wave 9"
  data$wave_no <- 9
  data$bhps_sample <- ifelse(!is.na(data$pid),TRUE,FALSE)
  data$dataset <- "UKHLS"
  data$id <- ifelse(data$bhps_sample==FALSE,data$pidp,data$pid)

  ######## ADD IN HOUSEHOLD DATA

  data.hhold <- data.table::fread(
    paste0(root[1], path, "ukhls_w9/i_hhresp.tab"),
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(data.hhold, names(data.hhold), tolower(names(data.hhold)))

  hhold_vars <- colnames(data.hhold[, c(1,333,320,271,315,321,322,323,324)])

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

  return(data_merged[])
}
