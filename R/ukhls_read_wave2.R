#' Read Understanding Society Wave 2
#'
#' Reads and does basic cleaning on the UKHLS second wave.
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
ukhls_read_wave2 <- function(
  root = c("C:/"),
  path = "Users/User/Documents/Datasets/UKHLS/tab/"
) {


  print("Reading UKHLS Wave 2")
  data <- data.table::fread(
    paste0(root[1], path, "ukhls_w2/b_indresp.tab"),
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-10", "-90", "-90.0", "N/A")
  )

  data.table::setnames(data, names(data), tolower(names(data)))

  id_vars          <- colnames(data[,c(1,2,3,4,7,8,80,81,82)])
  demographic_vars <- colnames(data[,c(13,14,15,1542,1543,92)])
  econ_stat_vars   <- colnames(data[,c(90)])
  work_vars        <- colnames(data[,c(1457,1458,770,1447)])
  education_vars   <- colnames(data[,c(1580)])
  health_vars      <- colnames(data[,c(242,272,1337,1377,1380)])
  preg_vars        <- colnames(data[,c(384,400,416,432,448)])
  smoke_vars       <- colnames(data[,c(235:241)])
  weight_vars      <- colnames(data[,c(1646,1647)])


  names <- c(id_vars,demographic_vars,econ_stat_vars,work_vars,education_vars,health_vars,preg_vars,smoke_vars,weight_vars)
  names <- tolower(names)

  data <- data[ , names, with = F]

  data.table::setnames(data,

                       c("pidp","pid","b_hidp","b_pno","b_psu","b_strata","b_istrtdaty","b_istrtdatm","b_istrtdatd",
                         ## demographic
                         "b_sex","b_dvage","b_birthy","b_gor_dv","b_urban_dv","b_mlstat",
                         ## economic stauts
                         "b_jbstat",
                         ## work variables
                         "b_paygu_dv","b_payg_dv","b_jbhrs","b_fimnlabgrs_dv",
                         ## education variables
                         "b_hiqual_dv",
                         ## health variables
                         "b_health","b_aidhh","b_scsf1","b_sclfsat1","b_sclfsato",
                         ## pregnancy variables
                         "b_pregout1","b_pregout2","b_pregout3","b_pregout4","b_pregout5",
                         ## smoking variables
                         "b_smever","b_smnow","b_ncigs","b_smcigs","b_smncigs","b_aglquit","b_smagbg",
                         ## weight
                         "b_indinus_lw","b_indinub_xw"),

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
                         "pregout1","pregout2","pregout3","pregout4","pregout5",
                         ## smoking variables
                         "smever","smnow","ncigs","smcigs","smncigs","aglquit","smagbg",
                         ## weight
                         "weight_lw","weight_xw"))

  data$wave <- "UKHLS Wave 2"
  data$wave_no <- 2
  data$bhps_sample <- ifelse(!is.na(data$pid),TRUE,FALSE)
  data$dataset <- "UKHLS"
  data$id <- ifelse(data$bhps_sample==FALSE,data$pidp,data$pid)

  ######## ADD IN HOUSEHOLD DATA

  data.hhold <- data.table::fread(
    paste0(root[1], path, "ukhls_w2/b_hhresp.tab"),
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(data.hhold, names(data.hhold), tolower(names(data.hhold)))

  hhold_vars <- colnames(data.hhold[, c(1,221,9,10,169,203,209,210,211,212)])

  data.hhold <- data.hhold[ , hhold_vars, with = F]
  data.table::setnames(data.hhold,
                       # old names
                       c("b_hidp","b_tenure_dv","b_numadult","b_nkids015","b_hhsize","b_hhtype_dv",
                         "b_nch02_dv","b_nch34_dv","b_nch511_dv","b_nch1215_dv"),
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
