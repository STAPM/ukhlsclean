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
#' @source University of Essex, Institute for Social and Economic Research. (2022). Understanding Society: Waves 1-12, 2009-2021
#' and Harmonised BHPS: Waves 1-18, 1991-2009. [data collection]. 17th Edition. UK Data Service. SN: 6614,
#' \href{https://beta.ukdataservice.ac.uk/datacatalogue/studies/study?id=6614}{DOI: 10.5255/UKDA-SN-6614-18}
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
ukhls_read_wave9 <- function(
  root = c("C:/"),
  file = "Users/cm1djm/Documents/Datasets/UKHLS/tab/",
  full = TRUE
) {

  cat(crayon::magenta("\tReading UKHLS Wave 9 datasets"))

  cat(crayon::red("\tIndividual..."))

  path <- here::here(paste0(root, file))

  data <- data.table::fread(
    paste0(path, "/i_indresp.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-10", "-90", "-90.0", "N/A")
  )
  if (full == TRUE) {
    # retain full interviews only
    data <- data[i_ivfio==1,]
  }

  data.table::setnames(data, names(data), tolower(names(data)))

  id_vars          <- Hmisc::Cs(pidp, pid, i_hidp, i_pno, i_psu, i_strata, i_istrtdaty, i_istrtdatm, i_istrtdatd)
  demographic_vars <- Hmisc::Cs(i_sex, i_dvage, i_birthy, i_gor_dv, i_urban_dv, i_mlstat, i_marstat)
  prev_wave_vars   <- Hmisc::Cs(i_notempchk, i_empchk)
  econ_stat_vars   <- Hmisc::Cs(i_jbstat, i_jbhas, i_jboff, i_jboffy, i_jbterm1, i_jbterm2, i_jbsemp)
  work_vars        <- Hmisc::Cs(i_paygu_dv, i_payg_dv, i_jbhrs, i_fimnlabgrs_dv, i_seearngrs_dv, i_jbsic07_cc, i_jbot, i_jbotpd)
  employees_vars   <- Hmisc::Cs(i_paygl, i_paynl, i_payu, i_payug, i_paytyp, i_ovtpay, i_pvtpyset, i_extrate, i_extrest, i_basnset, i_basrate, i_basrest, i_ovtnset, i_ovtrate, i_ovtrest)
  s.emp_vars       <- Hmisc::Cs(i_jshrs, i_jspayu, i_jspytx, i_jspyni)
  non.emp_vars     <- Hmisc::Cs(i_jbhad)
  job2_vars        <- Hmisc::Cs(i_j2has, i_j2semp, i_j2hrs, i_j2pay)
  benefits_vars    <- Hmisc::Cs(i_benbase1, i_benbase2, i_benbase3, i_benbase4, i_benbase96,
                                i_benctc)
  pension_vars     <- Hmisc::Cs(i_benpen1, i_benpen2, i_benpen3, i_benpen4, i_benpen5, i_benpen6, i_benpen7, i_benpen8, i_benpen96,
                                i_niserps)
  bendis_vars      <- Hmisc::Cs(i_bendis1, i_bendis2, i_bendis3, i_bendis4, i_bendis5, i_bendis12,
                                i_bendis7, i_bendis8, i_bendis10, i_bendis97, i_bendis96)
  otherben_vars    <- Hmisc::Cs(i_benesa,
                                i_othben1, i_othben2, i_othben3, i_othben4, i_othben5, i_othben6, i_othben7, i_othben8, i_othben9, i_othben97, i_othben96)
  benincome_vars   <- Hmisc::Cs(i_bensta2, i_bensta3, i_bensta4, i_bensta5, i_bensta6, i_bensta7, i_bensta97, i_bensta96)
  hhfinance_vars   <- Hmisc::Cs(i_fiyrdia, i_fiyrdb1, i_fiyrdb2, i_fiyrdb3, i_fiyrdb4, i_fiyrdb5, i_fiyrdb6, i_finnow, i_finfut)
  education_vars   <- Hmisc::Cs(i_hiqual_dv)
  health_vars      <- Hmisc::Cs(i_health, i_aidhh, i_sclfsat1, i_sclfsato, i_sf12pcs_dv, i_sf12mcs_dv,
                                i_scsf1, i_scsf2a, i_scsf2b, i_scsf3a, i_scsf3b, i_scsf4a, i_scsf4b, i_scsf5, i_scsf6a, i_scsf6b, i_scsf6c, i_scsf7)
  preg_vars        <- Hmisc::Cs(i_preg,
                                i_pregout1, i_pregend1, i_pregsmoke1, i_smkmnth11, i_smkmnth21, i_smkmnth31, i_pregsmk11, i_pregsmk21, i_pregsmk31, i_aedrof1, i_aepuwk1, i_aepuda1, i_lchmulti1,
                                i_pregout2, i_pregend2, i_pregsmoke2, i_smkmnth12, i_smkmnth22, i_smkmnth32, i_pregsmk12, i_pregsmk22, i_pregsmk32, i_aedrof2, i_aepuwk2, i_aepuda2, i_lchmulti2,
                                i_pregout3, i_pregend3, i_pregsmoke3, i_smkmnth13, i_smkmnth23, i_smkmnth33, i_pregsmk13, i_pregsmk23, i_pregsmk33, i_aedrof3, i_aepuwk3, i_aepuda3, i_lchmulti3,
                                i_nnewborn)
  smoke_vars       <- Hmisc::Cs(i_smoker, i_ncigs, i_giveup, i_gvupreas1, i_gvupreas2, i_gvupreas3, i_gvupreas4, i_gvupreas5, i_gvupreas6, i_gvupreas7, i_gvupreas8, i_gvupreas9, i_ecigs1)
  alc_vars         <- Hmisc::Cs(i_auditc1, i_auditc2, i_auditc3, i_auditc4, i_auditc5)
  weight_vars      <- Hmisc::Cs(i_indinus_lw, i_indinui_xw)


  names <- c(id_vars, demographic_vars, prev_wave_vars, econ_stat_vars, work_vars, employees_vars, s.emp_vars, non.emp_vars, job2_vars, benefits_vars, pension_vars, bendis_vars, otherben_vars, benincome_vars, hhfinance_vars, education_vars, health_vars, preg_vars, smoke_vars, alc_vars, weight_vars)
  names <- tolower(names)

  data <- data[ , names, with = F]

  data.table::setnames(data,

                       c("pidp","pid","i_hidp","i_pno","i_psu","i_strata","i_istrtdaty","i_istrtdatm","i_istrtdatd",
                         ## demographic
                         "i_sex","i_dvage","i_birthy","i_gor_dv","i_urban_dv","i_mlstat","i_marstat",
                         ## previous wave variables
                         "i_notempchk","i_empchk",
                         ## economic status
                         "i_jbstat","i_jbhas","i_jboff","i_jboffy","i_jbterm1","i_jbterm2","i_jbsemp",
                         ## work variables
                         "i_paygu_dv","i_payg_dv","i_jbhrs","i_fimnlabgrs_dv","i_seearngrs_dv","i_jbsic07_cc","i_jbot","i_jbotpd",
                         ## employees
                         "i_paygl","i_paynl","i_payu","i_payug","i_paytyp","i_ovtpay","i_pvtpyset","i_extrate","i_extrest","i_basnset","i_basrate",
                         "i_basrest","i_ovtnset","i_ovtrate","i_ovtrest",
                         ## self-employed
                         "i_jshrs","i_jspayu","i_jspytx","i_jspyni",
                         ## non-employed
                         "i_jbhad",
                         ## second job
                         "i_j2has","i_j2semp","i_j2hrs","i_j2pay",
                         ## benefits
                         "i_benbase1","i_benbase2","i_benbase3","i_benbase4","i_benbase96",
                         "i_benctc",
                         ## pensions
                         "i_benpen1","i_benpen2","i_benpen3","i_benpen4","i_benpen5","i_benpen6","i_benpen7","i_benpen8","i_benpen96",
                         "i_niserps",
                         ## disability benefits
                         "i_bendis1","i_bendis2","i_bendis3","i_bendis4","i_bendis5","i_bendis12",
                         "i_bendis7","i_bendis8","i_bendis10","i_bendis97","i_bendis96",
                         ## other benefits
                         "i_benesa",
                         "i_othben1","i_othben2","i_othben3","i_othben4","i_othben5","i_othben6","i_othben7","i_othben8","i_othben9","i_othben97","i_othben96",
                         ## benefit income variables (formerly receivables)
                         "i_bensta2","i_bensta3","i_bensta4","i_bensta5","i_bensta6","i_bensta7","i_bensta97","i_bensta96",
                         ## household finance variables (interest and dividends)
                         "i_fiyrdia","i_fiyrdb1","i_fiyrdb2","i_fiyrdb3","i_fiyrdb4","i_fiyrdb5","i_fiyrdb6","i_finnow","i_finfut",
                         ## education variables
                         "i_hiqual_dv",
                         ## health variables
                         "i_health","i_aidhh","i_sclfsat1","i_sclfsato","i_sf12pcs_dv","i_sf12mcs_dv",
                         "i_scsf1","i_scsf2a","i_scsf2b","i_scsf3a","i_scsf3b","i_scsf4a","i_scsf4b","i_scsf5","i_scsf6a","i_scsf6b","i_scsf6c","i_scsf7",
                         ## pregnancy variables
                         "i_preg",
                         "i_pregout1","i_pregend1","i_pregsmoke1","i_smkmnth11","i_smkmnth21","i_smkmnth31","i_pregsmk11","i_pregsmk21","i_pregsmk31","i_aedrof1","i_aepuwk1","i_aepuda1","i_lchmulti1",
                         "i_pregout2","i_pregend2","i_pregsmoke2","i_smkmnth12","i_smkmnth22","i_smkmnth32","i_pregsmk12","i_pregsmk22","i_pregsmk32","i_aedrof2","i_aepuwk2","i_aepuda2","i_lchmulti2",
                         "i_pregout3","i_pregend3","i_pregsmoke3","i_smkmnth13","i_smkmnth23","i_smkmnth33","i_pregsmk13","i_pregsmk23","i_pregsmk33","i_aedrof3","i_aepuwk3","i_aepuda3","i_lchmulti3",
                         "i_nnewborn",
                         ## smoke variables
                         "i_smoker", "i_ncigs", "i_giveup", "i_gvupreas1", "i_gvupreas2", "i_gvupreas3",
                         "i_gvupreas4", "i_gvupreas5", "i_gvupreas6", "i_gvupreas7", "i_gvupreas8", "i_gvupreas9", "i_ecigs1",
                         ## alcohol variables
                         "i_auditc1","i_auditc2","i_auditc3","i_auditc4","i_auditc5",
                         ## weight
                         "i_indinus_lw","i_indinui_xw"),

                       c("pidp","pid","hidp","person_number","psu","strata","year","month","day",
                         ## demographic
                         "sex","age","birth_year","region","urban","mlstat","marstat",
                         ## previous wave variables
                         "notempchk","empchk",
                         ## economic status
                         "econ_stat","jbhas","jboff","jboffy","jbterm1","jbterm2","jbsemp",
                         ## work variables
                         "grss_pay_usual","grss_pay_last","hours","grss_lab_inc","grss_semp","sic07","ovthours_pw","ovthours_paid",
                         ## employees
                         "last_gross_pay","last_net_pay","usual_pay","payug","pay_type","ovtpay","additional.pay_set","extrate","ext_estimate","basic.pay_set","baspay_rate",
                         "baspay_estimate","ovt.pay_set","ovtpay_rate","ovtpay_estimate",
                         ## self-employed
                         "s.emp_hours","s.emp_pay","s.emp_pay_pretax","s.emp_pay_preNI",
                         ## non-employed
                         "jbhad",
                         ## second job
                         "2ndjb","2ndjb_s.emp","2ndjb_hours","2ndjob_pay",
                         ## benefits
                         "incomesupp_ben","jbseek_allowance","ben_childben","universal_credit","no_benbase",
                         "ben_childtaxcred",
                         ## pensions
                         "NI.state_pen","employer_pen","spouse.emp_pen","pencred_pen","prvt_pen","widow_pen","parent_pen","war_pen","non_benpen",
                         "income_serps",
                         ## disability benefits
                         "incap_ben","empsupport_allowance","severedisab_allowance","carers_allowance","disliving_allowance","pers.indep_pay","attend_allowance",
                         "injury_ben","sick.accident_insurance","otherdis_pay","non_bendis",
                         ## other benefits
                         "employ_supp_allowance",
                         "foster_allowance","mat_allowance","inwork_cred","RTW_cred","workingtax_cred","counciltax_ben","rate_rebate","housing_ben","rent_rebate","othben_other","non_othben",
                         ## benefit income variables (formerly receivables)
                         "bensta_edugrant","bensta_tupay","bensta_alimony","bensta_fampay","bensta_rentlodge","bensta_rentother","bensta_other","non_bensta",
                         ## household finance variables
                         "fiyrdia","fiyrdb1","fiyrdb2","fiyrdb3","fiyrdb4","fiyrdb5","fiyrdb6","finnow","finfut",
                         ## education variables
                         "highest_qual",
                         ## health variables
                         "lt_sick","caring","health_satisf","life_satisf","sf12_pcs","sf12_mcs",
                         "sf1","sf2a","sf2b","sf3a","sf3b","sf4a","sf4b","sf5","sf6a","sf6b","sf6c","sf7",
                         ## pregnancy variables
                         "preg",
                         "pregout1","pregend1","pregsmoke1","smkmnth11","smkmnth21","smkmnth31","pregsmk_ncigs11","pregsmk_ncigs21","pregsmk_ncigs31","pregdrnk_freq1","pregdrnk_unitpw1","pregdrnk_unit1","lchmulti1",
                         "pregout2","pregend2","pregsmoke2","smkmnth12","smkmnth22","smkmnth32","pregsmk_ncigs12","pregsmk_ncigs22","pregsmk_ncigs32","pregdrnk_freq2","pregdrnk_unitpw2","pregdrnk_unit2","lchmulti2",
                         "pregout3","pregend3","pregsmoke3","smkmnth13","smkmnth23","smkmnth33","pregsmk_ncigs13","pregsmk_ncigs23","pregsmk_ncigs33","pregdrnk_freq3","pregdrnk_unitpw3","pregdrnk_unit3","lchmulti3",
                         "nnewborn",
                         ## smoke variables
                         "smoker", "ncigs", "giveup_smk", "gvupsmk_curr_health_prblm", "gvupsmk_general_better_health", "gvupsmk_risk_down_smk_illness",
                         "gvupsmk_public_work_smk_ban", "gvupsmk_fam&frnds", "gvupsmk_finances", "gvupsmk_chld_effect", "gvupsmk_fam_effect", "gvupsmk_other_reason", "ecigs",
                         ## alcohol variables
                         "auditc1","auditc2","auditc3","auditc4","auditc5",
                         ## weight
                         "weight_lw","weight_xw"))

  data[, wave := "UKHLS Wave 9"]
  data[, wave_no := 9]
  data[, bhps_sample := ifelse(!is.na(pid),TRUE,FALSE)]
  data[, dataset := "UKHLS"]
  data[, id := ifelse(bhps_sample==FALSE, pidp, pid)]

  ########################################
  ######## ADD IN HOUSEHOLD DATA #########

  cat(crayon::red("\tHousehold..."))

  data.hhold <- data.table::fread(
    paste0(path, "/i_hhresp.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(data.hhold, names(data.hhold), tolower(names(data.hhold)))

  hhold_vars          <- Hmisc::Cs(i_hidp, i_tenure_dv, i_nkids_dv, i_hhsize, i_hhtype_dv,
                                   i_nch02_dv, i_nch34_dv, i_nch511_dv, i_nch1215_dv)

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
                        by = "hidp",
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
