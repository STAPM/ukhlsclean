#' Read Understanding Society Wave 11
#'
#' Reads and does basic cleaning on the UKHLS eleventh wave.
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
ukhls_read_wave11 <- function(
  root = c("C:/"),
  file = "Users/cm1djm/Documents/Datasets/UKHLS/tab/",
  full = TRUE
) {

  cat(crayon::magenta("\tReading UKHLS Wave 11 datasets"))

  cat(crayon::red("\tIndividual..."))

  path <- here::here(paste0(root, file))

  data <- data.table::fread(
    paste0(path, "/k_indresp.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-10", "-90", "-90.0", "N/A")
  )
  if (full == TRUE) {
    # retain full interviews only
    data <- data[k_ivfio==1,]
  }

  data.table::setnames(data, names(data), tolower(names(data)))

  id_vars          <- Hmisc::Cs(pidp, pid, k_hidp, k_pno, k_psu, k_strata, k_istrtdaty, k_istrtdatm, k_istrtdatd)
  demographic_vars <- Hmisc::Cs(k_sex, k_dvage, k_birthy, k_gor_dv, k_urban_dv, k_mlstat, k_marstat)
  prev_wave_vars   <- Hmisc::Cs(k_notempchk, k_empchk)
  econ_stat_vars   <- Hmisc::Cs(k_jbstat, k_jbhas, k_jboff, k_jboffy, k_jbterm1, k_jbterm2, k_jbsemp)
  work_vars        <- Hmisc::Cs(k_paygu_dv, k_payg_dv, k_jbhrs, k_fimnlabgrs_dv, k_seearngrs_dv, k_jbsic07_cc, k_jbot, k_jbotpd)
  employees_vars   <- Hmisc::Cs(k_paygl, k_paynl, k_payu, k_payug, k_paytyp, k_ovtpay, k_pvtpyset, k_extrate, k_extrest, k_basnset, k_basrate, k_basrest, k_ovtnset, k_ovtrate, k_ovtrest)
  s.emp_vars       <- Hmisc::Cs(k_jshrs, k_jspayu, k_jspytx, k_jspyni)
  non.emp_vars     <- Hmisc::Cs(k_jbhad)
  job2_vars        <- Hmisc::Cs(k_j2has, k_j2semp, k_j2hrs, k_j2pay)
  benefits_vars    <- Hmisc::Cs(k_benbase1, k_benbase2, k_benbase3, k_benbase4, k_benbase96,
                                k_benctc)
  pension_vars     <- Hmisc::Cs(k_benpen1, k_benpen2, k_benpen3, k_benpen4, k_benpen5, k_benpen6, k_benpen7, k_benpen8, k_benpen96,
                                k_niserps)
  bendis_vars      <- Hmisc::Cs(k_bendis1, k_bendis2, k_bendis3, k_bendis4, k_bendis5, k_bendis12,
                                k_bendis7, k_bendis8, k_bendis10, k_bendis97, k_bendis96)
  otherben_vars    <- Hmisc::Cs(k_benesa,
                                k_othben1, k_othben2, k_othben3, k_othben4, k_othben5, k_othben6, k_othben7, k_othben8, k_othben9, k_othben97, k_othben96)
  benincome_vars   <- Hmisc::Cs(k_bensta2, k_bensta3, k_bensta4, k_bensta5, k_bensta6, k_bensta7, k_bensta97, k_bensta96)
  hhfinance_vars   <- Hmisc::Cs(k_fiyrdia, k_fiyrdb1, k_fiyrdb2, k_fiyrdb3, k_fiyrdb4, k_fiyrdb5, k_fiyrdb6, k_finnow, k_finfut)
  education_vars   <- Hmisc::Cs(k_hiqual_dv)
  health_vars      <- Hmisc::Cs(k_health, k_aidhh, k_sclfsat1, k_sclfsato, k_sf12pcs_dv, k_sf12mcs_dv,
                                k_scsf1, k_scsf2a, k_scsf2b, k_scsf3a, k_scsf3b, k_scsf4a, k_scsf4b, k_scsf5, k_scsf6a,
                                k_scsf6b, k_scsf6c, k_scsf7)
  preg_vars        <- Hmisc::Cs(k_preg,
                                k_pregout1, k_pregend1, k_pregsmoke1, k_smkmnth11, k_smkmnth21, k_smkmnth31, k_pregsmk11, k_pregsmk21, k_pregsmk31, k_aedrof1, k_aepuwk1, k_aepuda1, k_lchmulti1,
                                k_pregout2, k_pregend2, k_pregsmoke2, k_smkmnth12, k_smkmnth22, k_smkmnth32, k_pregsmk12, k_pregsmk22, k_pregsmk32, k_aedrof2, k_aepuwk2, k_aepuda2, k_lchmulti2,
                                k_pregout3, k_pregend3, k_pregsmoke3, k_smkmnth13, k_smkmnth23, k_smkmnth33, k_pregsmk13, k_pregsmk23, k_pregsmk33, k_aedrof3, k_aepuwk3, k_aepuda3, k_lchmulti3,
                                k_nnewborn)
  smoke_vars       <- Hmisc::Cs(k_smoker, k_ncigs, k_giveup, k_gvupreas1, k_gvupreas2, k_gvupreas3, k_gvupreas4, k_gvupreas5, k_gvupreas6, k_gvupreas7, k_gvupreas8, k_gvupreas9, k_ecigs1)
  alc_vars         <- Hmisc::Cs(k_auditc1, k_auditc2, k_auditc3, k_auditc4, k_auditc5)
  weight_vars      <- Hmisc::Cs(k_indinus_lw, k_indinui_xw)

  names <- c(id_vars, demographic_vars, prev_wave_vars, econ_stat_vars, work_vars, employees_vars, s.emp_vars, non.emp_vars, job2_vars,
             benefits_vars, pension_vars, bendis_vars, otherben_vars, benincome_vars, hhfinance_vars, education_vars, health_vars, preg_vars, smoke_vars, alc_vars, weight_vars)
  names <- tolower(names)

  data <- data[ , names, with = F]

  data.table::setnames(data,

                       c("pidp","pid","k_hidp","k_pno","k_psu","k_strata","k_istrtdaty","k_istrtdatm","k_istrtdatd",
                         ## demographic
                         "k_sex","k_dvage","k_birthy","k_gor_dv","k_urban_dv","k_mlstat","k_marstat",
                         ## previous wave variables
                         "k_notempchk","k_empchk",
                         ## economic status
                         "k_jbstat","k_jbhas","k_jboff","k_jboffy","k_jbterm1","k_jbterm2","k_jbsemp",
                         ## work variables
                         "k_paygu_dv","k_payg_dv","k_jbhrs","k_fimnlabgrs_dv","k_seearngrs_dv","k_jbsic07_cc","k_jbot","k_jbotpd",
                         ## employees
                         "k_paygl","k_paynl","k_payu","k_payug","k_paytyp","k_ovtpay","k_pvtpyset","k_extrate","k_extrest","k_basnset","k_basrate",
                         "k_basrest","k_ovtnset","k_ovtrate","k_ovtrest",
                         ## self-employed
                         "k_jshrs","k_jspayu","k_jspytx","k_jspyni",
                         ## non-employed
                         "k_jbhad",
                         ## second job
                         "k_j2has","k_j2semp","k_j2hrs","k_j2pay",
                         ## benefits
                         "k_benbase1","k_benbase2","k_benbase3","k_benbase4","k_benbase96",
                         "k_benctc",
                         ## pensions
                         "k_benpen1","k_benpen2","k_benpen3","k_benpen4","k_benpen5","k_benpen6","k_benpen7","k_benpen8","k_benpen96",
                         "k_niserps",
                         ## disability benefits
                         "k_bendis1","k_bendis2","k_bendis3","k_bendis4","k_bendis5","k_bendis12",
                         "k_bendis7","k_bendis8","k_bendis10","k_bendis97","k_bendis96",
                         ## other benefits
                         "k_benesa",
                         "k_othben1","k_othben2","k_othben3","k_othben4","k_othben5","k_othben6","k_othben7","k_othben8","k_othben9","k_othben97","k_othben96",
                         ## benefit income variables (formerly receivables)
                         "k_bensta2","k_bensta3","k_bensta4","k_bensta5","k_bensta6","k_bensta7","k_bensta97","k_bensta96",
                         ## household finance variables (interest and dividends)
                         "k_fiyrdia","k_fiyrdb1","k_fiyrdb2","k_fiyrdb3","k_fiyrdb4","k_fiyrdb5","k_fiyrdb6","k_finnow","k_finfut",
                         ## education variables
                         "k_hiqual_dv",
                         ## health variables
                         "k_health","k_aidhh","k_sclfsat1","k_sclfsato","k_sf12pcs_dv","k_sf12mcs_dv",
                         "k_scsf1","k_scsf2a","k_scsf2b","k_scsf3a","k_scsf3b","k_scsf4a","k_scsf4b","k_scsf5","k_scsf6a","k_scsf6b","k_scsf6c","k_scsf7",
                         ## pregnancy variables
                         "k_preg",
                         "k_pregout1","k_pregend1","k_pregsmoke1","k_smkmnth11","k_smkmnth21","k_smkmnth31","k_pregsmk11","k_pregsmk21","k_pregsmk31","k_aedrof1","k_aepuwk1","k_aepuda1","k_lchmulti1",
                         "k_pregout2","k_pregend2","k_pregsmoke2","k_smkmnth12","k_smkmnth22","k_smkmnth32","k_pregsmk12","k_pregsmk22","k_pregsmk32","k_aedrof2","k_aepuwk2","k_aepuda2","k_lchmulti2",
                         "k_pregout3","k_pregend3","k_pregsmoke3","k_smkmnth13","k_smkmnth23","k_smkmnth33","k_pregsmk13","k_pregsmk23","k_pregsmk33","k_aedrof3","k_aepuwk3","k_aepuda3","k_lchmulti3",
                         "k_nnewborn",
                         ## smoke variables
                         "k_smoker", "k_ncigs", "k_giveup", "k_gvupreas1", "k_gvupreas2", "k_gvupreas3",
                         "k_gvupreas4", "k_gvupreas5", "k_gvupreas6", "k_gvupreas7", "k_gvupreas8", "k_gvupreas9", "k_ecigs1",
                         ## alcohol variables
                         "k_auditc1","k_auditc2","k_auditc3","k_auditc4","k_auditc5",
                         ## weight
                         "k_indinus_lw","k_indinui_xw"),

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

  data[, wave := "UKHLS Wave 11"]
  data[, wave_no := 11]
  data[, bhps_sample := ifelse(!is.na(pid),TRUE,FALSE)]
  data[, dataset := "UKHLS"]
  data[, id := ifelse(bhps_sample==FALSE, pidp, pid)]

  ########################################
  ######## ADD IN HOUSEHOLD DATA #########

  cat(crayon::red("\tHousehold..."))

  data.hhold <- data.table::fread(
    paste0(path, "/k_hhresp.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(data.hhold, names(data.hhold), tolower(names(data.hhold)))

  hhold_vars          <- Hmisc::Cs(k_hidp, k_tenure_dv, k_nkids_dv, k_hhsize, k_hhtype_dv,
                                   k_nch02_dv, k_nch34_dv, k_nch511_dv, k_nch1215_dv)

  data.hhold <- data.hhold[ , hhold_vars, with = F]
  data.table::setnames(data.hhold,
                       # old names
                       c("k_hidp", "k_tenure_dv", "k_nkids_dv", "k_hhsize","k_hhtype_dv",
                         "k_nch02_dv", "k_nch34_dv", "k_nch511_dv", "k_nch1215_dv"),
                       # new names
                       c("hidp", "hh_tenure", "hh_numchild", "hh_size", "hh_type",
                         "hh_numchild02", "hh_numchild34", "hh_numchild511", "hh_numchild1215"))

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
