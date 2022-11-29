#' Read Understanding Society Wave 8
#'
#' Reads and does basic cleaning on the UKHLS eighth wave.
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
ukhls_read_wave8 <- function(
  root = c("C:/"),
  file = "Users/cm1djm/Documents/Datasets/UKHLS/tab/",
  full = TRUE
) {

  cat(crayon::magenta("\tReading UKHLS Wave 8 datasets"))

  cat(crayon::red("\tIndividual..."))

  path <- here::here(paste0(root, file))

  data <- data.table::fread(
    paste0(path, "/h_indresp.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-10", "-90", "-90.0", "N/A")
  )
  if (full == TRUE) {
  # retain full interviews only
  data <- data[h_ivfio==1,]
  }

  data.table::setnames(data, names(data), tolower(names(data)))

  id_vars          <- Hmisc::Cs(pidp,pid, h_hidp, h_pno, h_psu, h_strata, h_istrtdaty, h_istrtdatm,h_istrtdatd)
  demographic_vars <- Hmisc::Cs(h_sex, h_dvage, h_birthy, h_gor_dv, h_urban_dv, h_mlstat, h_marstat)
  prev_wave_vars   <- Hmisc::Cs(h_notempchk, h_empchk)
  econ_stat_vars   <- Hmisc::Cs(h_jbstat, h_jbhas, h_jboff, h_jboffy, h_jbterm1, h_jbterm2, h_jbsemp, h_jbpen, h_jbpenm)
  work_vars        <- Hmisc::Cs(h_paygu_dv, h_payg_dv, h_jbhrs, h_fimnlabgrs_dv, h_seearngrs_dv, h_jbsic07_cc, h_jbot, h_jbotpd)
  employees_vars   <- Hmisc::Cs(h_paygl, h_paynl, h_payu, h_payug, h_ovtpay, h_extrate, h_extrest, h_basrate, h_basrest, h_ovtrate, h_ovtrest)
  s.emp_vars       <- Hmisc::Cs(h_jshrs, h_jspayu, h_jspytx, h_jspyni)
  non.emp_vars     <- Hmisc::Cs(h_jbhad)
  job2_vars        <- Hmisc::Cs(h_j2has, h_j2semp, h_j2hrs, h_j2pay)
  benefits_vars    <- Hmisc::Cs(h_bendis1, h_bendis2, h_bendis3, h_bendis4, h_bendis5, h_bendis12,
                                h_bendis7, h_bendis8, h_bendis10, h_bendis97, h_bendis96)
  pension_vars     <- Hmisc::Cs(h_benpen1, h_benpen2, h_benpen3, h_benpen4, h_benpen5, h_benpen6, h_benpen7, h_benpen8, h_benpen96)
  receivables_vars <- Hmisc::Cs(h_niserps, h_benctc,
                                h_bensta2, h_bensta3, h_bensta4, h_bensta5, h_bensta6, h_bensta7, h_bensta97, h_bensta96)
  hhfinance_vars   <- Hmisc::Cs(h_fiyrdia, h_fiyrdb1, h_fiyrdb2, h_fiyrdb3, h_fiyrdb4, h_fiyrdb5, h_fiyrdb6, h_finnow, h_finfut)
  education_vars   <- Hmisc::Cs(h_hiqual_dv)
  health_vars      <- Hmisc::Cs(h_health, h_aidhh, h_sclfsat1, h_sclfsato, h_sf12pcs_dv, h_sf12mcs_dv,
                                h_scsf1, h_scsf2a, h_scsf2b, h_scsf3a, h_scsf3b, h_scsf4a, h_scsf4b, h_scsf5, h_scsf6a, h_scsf6b,h_scsf6c, h_scsf7)
  preg_vars        <- Hmisc::Cs(h_pregout1, h_pregout2, h_pregout3, h_pregout4)
  smoke_vars       <- Hmisc::Cs(h_smoker, h_ncigs)
  alc_vars         <- Hmisc::Cs(h_dklm, h_drnk4w, h_evralc, h_fivealcdr)
  weight_vars      <- Hmisc::Cs(h_indinus_lw, h_indinui_xw)


  names <- c(id_vars, demographic_vars, prev_wave_vars, econ_stat_vars, work_vars, employees_vars, s.emp_vars, non.emp_vars, job2_vars, benefits_vars, pension_vars, receivables_vars, hhfinance_vars, education_vars, health_vars, preg_vars, smoke_vars, alc_vars, weight_vars)
  names <- tolower(names)

  data <- data[ , names, with = F]

  data.table::setnames(data,

                       c("pidp","pid","h_hidp","h_pno","h_psu","h_strata","h_istrtdaty","h_istrtdatm","h_istrtdatd",
                         ## demographic
                         "h_sex","h_dvage","h_birthy","h_gor_dv","h_urban_dv","h_mlstat","h_marstat",
                         ## previous wave variables
                         "h_notempchk","h_empchk",
                         ## economic status
                         "h_jbstat","h_jbhas","h_jboff","h_jboffy","h_jbterm1","h_jbterm2","h_jbsemp","h_jbpen","h_jbpenm",
                         ## work variables
                         "h_paygu_dv","h_payg_dv","h_jbhrs","h_fimnlabgrs_dv","h_seearngrs_dv","h_jbsic07_cc","h_jbot","h_jbotpd",
                         ## employees
                         "h_paygl","h_paynl","h_payu","h_payug","h_ovtpay","h_extrate","h_extrest","h_basrate",
                         "h_basrest","h_ovtrate","h_ovtrest",
                         ## self-employed
                         "h_jshrs","h_jspayu","h_jspytx","h_jspyni",
                         ## non-employed
                         "h_jbhad",
                         ## second job
                         "h_j2has","h_j2semp","h_j2hrs","h_j2pay",
                         ## benefits
                         "h_bendis1","h_bendis2","h_bendis3","h_bendis4","h_bendis5","h_bendis12",
                         "h_bendis7","h_bendis8","h_bendis10","h_bendis97","h_bendis96",
                         ## pensions
                         "h_benpen1","h_benpen2","h_benpen3","h_benpen4","h_benpen5","h_benpen6","h_benpen7","h_benpen8","h_benpen96",
                         ## receivables
                         "h_niserps","h_benctc",
                         "h_bensta2","h_bensta3","h_bensta4","h_bensta5","h_bensta6","h_bensta7","h_bensta97","h_bensta96",
                         ## household finance variables (interest and dividends)
                         "h_fiyrdia","h_fiyrdb1","h_fiyrdb2","h_fiyrdb3","h_fiyrdb4","h_fiyrdb5","h_fiyrdb6","h_finnow","h_finfut",
                         ## education variables
                         "h_hiqual_dv",
                         ## health variables
                         "h_health","h_aidhh","h_sclfsat1","h_sclfsato","h_sf12pcs_dv","h_sf12mcs_dv",
                         "h_scsf1","h_scsf2a","h_scsf2b","h_scsf3a","h_scsf3b","h_scsf4a","h_scsf4b","h_scsf5","h_scsf6a","h_scsf6b","h_scsf6c","h_scsf7",
                         ## pregnancy variables
                         "h_pregout1","h_pregout2","h_pregout3","h_pregout4",
                         ## smoking variables
                         "h_smoker", "h_ncigs",
                         ## alcohol variables
                         "h_dklm","h_drnk4w","h_evralc","h_fivealcdr",
                         ## weight
                         "h_indinus_lw","h_indinui_xw"),

                       c("pidp","pid","hidp","person_number","psu","strata","year","month","day",
                         ## demographic
                         "sex","age","birth_year","region","urban","mlstat","marstat",
                         ## previous wave variables
                         "notempchk","empchk",
                         ## economic status
                         "econ_stat","jbhas","jboff","jboffy","jbterm1","jbterm2","jbsemp","jbpen","jbpen_member",
                         ## work variables
                         "grss_pay_usual","grss_pay_last","hours","grss_lab_inc","grss_semp","sic07","ovthours_pw","ovthours_paid",
                         ## employees
                         "last_gross_pay","last_net_pay","usual_pay","payug","ovtpay","extrate","ext_estimate","baspay_rate",
                         "baspay_estimate","ovtpay_rate","ovtpay_estimate",
                         ## self-employed
                         "s.emp_hours","s.emp_pay","s.emp_pay_pretax","s.emp_pay_preNI",
                         ## non-employed
                         "jbhad",
                         ## second job
                         "2ndjb","2ndjb_s.emp","2ndjb_hours","2ndjob_pay",
                         ## benefits
                         "incap_ben","empsupport_allowance","severedisab_allowance","carers_allowance","disliving_allowance","pers.indep_pay","attend_allowance",
                         "injury_ben","sick.accident_insurance","otherdis_pay","non_bendis",
                         ## pensions
                         "NI.state_pen","employer_pen","spouse.emp_pen","pencred_pen","prvt_pen","widow_pen","parent_pen","war_pen","non_benpen",
                         ## receivables
                         "income_serps","ben_childtaxcred",
                         "bensta_edugrant","bensta_tupay","bensta_alimony","bensta_fampay","bensta_rentlodge","bensta_rentother","bensta_other","non_bensta",
                         ## household finance variables
                         "fiyrdia","fiyrdb1","fiyrdb2","fiyrdb3","fiyrdb4","fiyrdb5","fiyrdb6","finnow","finfut",
                         ## education variables
                         "highest_qual",
                         ## health variables
                         "lt_sick","caring","health_satisf","life_satisf","sf12_pcs","sf12_mcs",
                         "sf1","sf2a","sf2b","sf3a","sf3b","sf4a","sf4b","sf5","sf6a","sf6b","sf6c","sf7",
                         ## pregnancy variables
                         "pregout1","pregout2","pregout3","pregout4",
                         ## smoking variables
                         "smoker", "ncigs",
                         ## alcohol variables
                         "dklm","drnk4w","evralc","fivealcdr",
                         ## weight
                         "weight_lw","weight_xw"))

  data[, wave := "UKHLS Wave 8"]
  data[, wave_no := 8]
  data[, bhps_sample := ifelse(!is.na(pid),TRUE,FALSE)]
  data[, dataset := "UKHLS"]
  data[, id := ifelse(bhps_sample==FALSE, pidp, pid)]

  ########################################
  ######## ADD IN HOUSEHOLD DATA #########

  cat(crayon::red("\tHousehold..."))

  data.hhold <- data.table::fread(
    paste0(path, "/h_hhresp.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(data.hhold, names(data.hhold), tolower(names(data.hhold)))

  hhold_vars          <- Hmisc::Cs(h_hidp, h_tenure_dv, h_nkids_dv, h_hhsize, h_hhtype_dv,
                                   h_nch02_dv, h_nch34_dv, h_nch511_dv, h_nch1215_dv)

  data.hhold <- data.hhold[ , hhold_vars, with = F]
  data.table::setnames(data.hhold,
                       # old names
                       c("h_hidp","h_tenure_dv","h_nkids_dv","h_hhsize","h_hhtype_dv",
                         "h_nch02_dv","h_nch34_dv","h_nch511_dv","h_nch1215_dv"),
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
