#' Read Understanding Society Wave 4
#'
#' Reads and does basic cleaning on the UKHLS fourth wave.
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
ukhls_read_wave4 <- function(
  root = c("C:/"),
  path = "Users/cm1djm/Documents/Datasets/UKHLS/tab/",
  full = TRUE
) {


  print("Reading UKHLS Wave 4")
  data <- data.table::fread(
    paste0(root[1], path, "ukhls_w4/d_indresp.tab"),
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-10", "-90", "-90.0", "N/A")
  )
  if (full == TRUE) {
    # retain full interviews only
    data <- data[d_ivfio==1,]
  }

  data.table::setnames(data, names(data), tolower(names(data)))

  id_vars          <- colnames(data[,c(1,2,3,4,8,9,108,109,110)])
  demographic_vars <- colnames(data[,c(14,15,16,1988,1989,126)])
  econ_stat_vars   <- colnames(data[,c(124)])
  work_vars        <- colnames(data[,c(1900,1901,792,1890)])
  education_vars   <- colnames(data[,c(2026)])
  health_vars      <- colnames(data[,c(253,345,1575,1599,1602)])
  preg_vars        <- colnames(data[,c(445,463,481,499)])
  weight_vars      <- colnames(data[,c(2095,2096)])


  names <- c(id_vars,demographic_vars,econ_stat_vars,work_vars,education_vars,health_vars,preg_vars,weight_vars)
  names <- tolower(names)

  data <- data[ , names, with = F]

  data.table::setnames(data,

                       c("pidp","pid","d_hidp","d_pno","d_psu","d_strata","d_istrtdaty","d_istrtdatm","d_istrtdatd",
                         ## demographic
                         "d_sex","d_dvage","d_birthy","d_gor_dv","d_urban_dv","d_mlstat",
                         ## economic stauts
                         "d_jbstat",
                         ## work variables
                         "d_paygu_dv","d_payg_dv","d_jbhrs","d_fimnlabgrs_dv",
                         ## education variables
                         "d_hiqual_dv",
                         ## health variables
                         "d_health","d_aidhh","d_scsf1","d_sclfsat1","d_sclfsato",
                         ## pregnancy variables
                         "d_pregout1","d_pregout2","d_pregout3","d_pregout4",
                         ## weight
                         "d_indinus_lw","d_indinub_xw"),

                       c("pidp","pid","hidp","person_number","psu","strata","year","month","day",
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
                         ## pregnancy variables
                         "pregout1","pregout2","pregout3","pregout4",
                         ## weight
                         "weight_lw","weight_xw"))

  data$wave <- "UKHLS Wave 4"
  data$wave_no <- 4
  data$bhps_sample <- ifelse(!is.na(data$pid),TRUE,FALSE)
  data$dataset <- "UKHLS"
  data$id <- ifelse(data$bhps_sample==FALSE,data$pidp,data$pid)

  ######## ADD IN HOUSEHOLD DATA

  data.hhold <- data.table::fread(
    paste0(root[1], path, "ukhls_w4/d_hhresp.tab"),
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(data.hhold, names(data.hhold), tolower(names(data.hhold)))

  hhold_vars <- colnames(data.hhold[, c(1,524,10,12,466,506,512,513,514,515)])

  data.hhold <- data.hhold[ , hhold_vars, with = F]
  data.table::setnames(data.hhold,
                       # old names
                       c("d_hidp","d_tenure_dv","d_numadult","d_nkids015","d_hhsize","d_hhtype_dv",
                         "d_nch02_dv","d_nch34_dv","d_nch511_dv","d_nch1215_dv"),
                       # new names
                       c("hidp","hh_tenure","hh_numadult","hh_numchild","hh_size","hh_type",
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
