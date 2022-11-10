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
ukhls_read_wave4 <- function(
  root = c("C:/"),
  file = "Users/cm1djm/Documents/Datasets/UKHLS/tab/",
  full = TRUE
) {

  cat(crayon::magenta("\tReading UKHLS Wave 4 datasets"))

  cat(crayon::red("\tIndividual..."))

  path <- here::here(paste0(root, file))

  data <- data.table::fread(
    paste0(path, "/d_indresp.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-10", "-90", "-90.0", "N/A")
  )
  if (full == TRUE) {
    # retain full interviews only
    data <- data[d_ivfio==1,]
  }

  data.table::setnames(data, names(data), tolower(names(data)))

  id_vars          <- Hmisc::Cs(pidp, pid, d_hidp, d_pno, d_psu, d_strata, d_istrtdaty, d_istrtdatm, d_istrtdatd)
  demographic_vars <- Hmisc::Cs(d_sex, d_dvage, d_birthy, d_gor_dv, d_urban_dv, d_mlstat,  d_marstat)
  prev_wave_vars   <- Hmisc::Cs(d_notempchk, d_empchk)
  econ_stat_vars   <- Hmisc::Cs(d_jbstat, d_jbhas, d_jboff, d_jboffy, d_jbterm1, d_jbterm2, d_jbsemp, d_jbpen, d_jbpenm)
  work_vars        <- Hmisc::Cs(d_paygu_dv, d_payg_dv, d_jbhrs, d_fimnlabgrs_dv, d_seearngrs_dv, d_jbot, d_jbotpd)
  employees_vars   <- Hmisc::Cs(d_paygl, d_paynl, d_payu, d_payug, d_ovtpay, d_extnsa, d_extrate, d_extrest, d_basnsa, d_basrate, d_basrest, d_ovtnsa, d_ovtrate, d_ovtrest)
  s.emp_vars       <- Hmisc::Cs(d_jshrs, d_jspayu, d_jspytx, d_jspyni)
  non.emp_vars     <- Hmisc::Cs(d_jbhad)
  job2_vars        <- Hmisc::Cs(d_j2has, d_j2semp, d_j2hrs, d_j2pay)
  benefits_vars    <- Hmisc::Cs(d_btype1, d_btype2, d_btype3, d_btype4, d_btype5, d_btype6, d_btype7, d_btype8, d_btype9, d_btype96,
                                d_benunemp1, d_benunemp2, d_benunemp96, d_bendis1, d_bendis11, d_bendis2, d_bendis3, d_bendis4, d_bendis5, d_bendis12,
                                d_bendis6, d_bendis7, d_bendis8, d_bendis9, d_bendis10, d_bendis96, d_bendis97)
  education_vars   <- Hmisc::Cs(d_hiqual_dv)
  health_vars      <- Hmisc::Cs(d_health, d_aidhh, d_sclfsat1, d_sclfsato, d_sf12pcs_dv, d_sf12mcs_dv,
                                d_scsf1, d_scsf2a, d_scsf2b, d_scsf3a, d_scsf3b, d_scsf4a, d_scsf4b, d_scsf5, d_scsf6a, d_scsf6b, d_scsf6c, d_scsf7)
  preg_vars        <- Hmisc::Cs(d_pregout1, d_pregout2, d_pregout3, d_pregout4)
  alc_vars         <- Hmisc::Cs(d_dklm, d_drnk4w, d_evralc, d_fivealcdr)
  weight_vars      <- Hmisc::Cs(d_indinus_lw, d_indinub_xw)


  names <- c(id_vars, demographic_vars, prev_wave_vars, econ_stat_vars, work_vars, employees_vars, s.emp_vars, non.emp_vars, job2_vars, benefits_vars, education_vars, health_vars, preg_vars, alc_vars, weight_vars)
  names <- tolower(names)

  data <- data[ , names, with = F]

  data.table::setnames(data,

                       c("pidp","pid","d_hidp","d_pno","d_psu","d_strata","d_istrtdaty","d_istrtdatm","d_istrtdatd",
                         ## demographic
                         "d_sex","d_dvage","d_birthy","d_gor_dv","d_urban_dv","d_mlstat","d_marstat",
                         ## previous wave variables
                         "d_notempchk","d_empchk",
                         ## economic status
                         "d_jbstat","d_jbhas","d_jboff","d_jboffy","d_jbterm1","d_jbterm2","d_jbsemp","d_jbpen","d_jbpenm",
                         ## work variables
                         "d_paygu_dv","d_payg_dv","d_jbhrs","d_fimnlabgrs_dv","d_seearngrs_dv","d_jbot","d_jbotpd",
                         ## employees
                         "d_paygl","d_paynl","d_payu","d_payug","d_ovtpay","d_extnsa","d_extrate","d_extrest","d_basnsa","d_basrate",
                         "d_basrest","d_ovtnsa","d_ovtrate","d_ovtrest",
                         ## self-employed
                         "d_jshrs","d_jspayu","d_jspytx","d_jspyni",
                         ## non-employed
                         "d_jbhad",
                         ## second job
                         "d_j2has","d_j2semp","d_j2hrs","d_j2pay",
                         ## benefits
                         "d_btype1","d_btype2","d_btype3","d_btype4","d_btype5","d_btype6","d_btype7","d_btype8","d_btype9","d_btype96",
                         "d_benunemp1","d_benunemp2","d_benunemp96","d_bendis1","d_bendis11","d_bendis2","d_bendis3","d_bendis4","d_bendis5","d_bendis12",
                         "d_bendis6","d_bendis7","d_bendis8","d_bendis9","d_bendis10","d_bendis96","d_bendis97",
                         ## education variables
                         "d_hiqual_dv",
                         ## health variables
                         "d_health","d_aidhh","d_sclfsat1","d_sclfsato","d_sf12pcs_dv","d_sf12mcs_dv",
                         "d_scsf1","d_scsf2a","d_scsf2b","d_scsf3a","d_scsf3b","d_scsf4a","d_scsf4b","d_scsf5","d_scsf6a","d_scsf6b","d_scsf6c","d_scsf7",
                         ## pregnancy variables
                         "d_pregout1","d_pregout2","d_pregout3","d_pregout4",
                         ## alcohol variables
                         "d_dklm","d_drnk4w","d_evralc","d_fivealcdr",
                         ## weight
                         "d_indinus_lw","d_indinub_xw"),

                       c("pidp","pid","hidp","person_number","psu","strata","year","month","day",
                         ## demographic
                         "sex","age","birth_year","region","urban","mlstat","marstat",
                         ## previous wave variables
                         "notempchk","empchk",
                         ## economic status
                         "econ_stat","jbhas","jboff","jboffy","jbterm1","jbterm2","jbsemp","jbpen","jbpen_member",
                         ## work variables
                         "grss_pay_usual","grss_pay_last","hours","grss_lab_inc","grss_semp","ovthours_pw","ovthours_paid",
                         ## employees
                         "last_gross_pay","last_net_pay","usual_pay","payug","ovtpay","extnsa","extrate","ext_estimate","baspay_amount","baspay_rate",
                         "baspay_estimate","ovtpay_amount","ovtpay_rate","ovtpay_estimate",
                         ## self-employed
                         "s.emp_hours","s.emp_pay","s.emp_pay_pretax","s.emp_pay_preNI",
                         ## non-employed
                         "jbhad",
                         ## second job
                         "2ndjb","2ndjb_s.emp","2ndjb_hours","2ndjob_pay",
                         ## benefits
                         "unemp_ben","incomesupp_ben","sickdis_ben","pension_ben","child_ben","taxcred_ben","family_ben","counciltax_ben","otherstate_ben","no_ben",
                         "jbseek_allowance","NI_credits","non_btype1","incap_ben","universal_cred","empsupport_allowance","severedisab_allowance","carers_allowance","disliving_allowance","pers.indep_pay",
                         "RTW_credit","attend_allowance","injury_ben","war_pension","sick.accident_insurance","non_bendis","otherdis_pay",
                         ## education variables
                         "highest_qual",
                         ## health variables
                         "lt_sick","caring","health_satisf","life_satisf","sf12_pcs","sf12_mcs",
                         "sf1","sf2a","sf2b","sf3a","sf3b","sf4a","sf4b","sf5","sf6a","sf6b","sf6c","sf7",
                         ## pregnancy variables
                         "pregout1","pregout2","pregout3","pregout4",
                         ## alcohol variables
                         "dklm","drnk4w","evralc","fivealcdr",
                         ## weight
                         "weight_lw","weight_xw"))

  data[, wave := "UKHLS Wave 4"]
  data[, wave_no := 4]
  data[, bhps_sample := ifelse(!is.na(pid),TRUE,FALSE)]
  data[, dataset := "UKHLS"]
  data[, id := ifelse(bhps_sample==FALSE, pidp, pid)]

  ########################################
  ######## ADD IN HOUSEHOLD DATA #########

  cat(crayon::red("\tHousehold..."))

  data.hhold <- data.table::fread(
    paste0(path, "/d_hhresp.tab"),
    showProgress = FALSE,
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
