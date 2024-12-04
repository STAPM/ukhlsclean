#' Read Understanding Society Wave 12
#'
#' Reads and performs basic cleaning operations on the UKHLS twelfth wave. Missing values as detailed below are all set to NA.
#'
#' MISSING VALUES
#'
#' \itemize{
#' \item -1 Don't know. When the respondent does not know the answer to a question.
#' \item -2 Refused: When the respondent refuses to answer a question.
#' \item -7 Proxy: A question not included in the subset of questions asked of proxy respondents.
#' obtained or not attempted.
#' \item -8 Not applicable: Used to signify that a particular variable did not apply to a given respondent
#' usually because of internal routing. For example, men in women only questions.
#' \item -9 Missing by error or implausible answer.
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
ukhls_read_wave12 <- function(
  root = c("X:/"),
  file = "HAR_PR/PR/USoc/Data/SN6614_2024_12_02/tab/ukhls/",
  full = TRUE
) {

  cat(crayon::blue(crayon::underline("\tReading UKHLS Wave 12 datasets")))

  cat(crayon::green("\tIndividual..."))

  path <- here::here(paste0(root, file))

  data <- data.table::fread(
    paste0(path, "/l_indresp.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-10", "-90", "-90.0", "N/A")
  )
  if (full == TRUE) {
    # retain full interviews only
    data <- data[l_ivfio==1,]
  }

  data.table::setnames(data, names(data), tolower(names(data)))

  id_vars          <- Hmisc::Cs(pidp, pid, l_hidp, l_pno, l_psu, l_strata, l_istrtdaty, l_istrtdatm, l_istrtdatd)
  demographic_vars <- Hmisc::Cs(l_sex, l_dvage, l_birthy, l_gor_dv, l_urban_dv, l_mlstat, l_marstat)
  prev_wave_vars   <- Hmisc::Cs(l_notempchk, l_empchk)
  econ_stat_vars   <- Hmisc::Cs(l_jbstat, l_jbhas, l_jboff, l_jboffy, l_jbterm1, l_jbterm2, l_jbsemp, l_jbpen, l_jbpenm)
  work_vars        <- Hmisc::Cs(l_paygu_dv, l_payg_dv, l_jbhrs, l_fimnlabgrs_dv, l_seearngrs_dv, l_jbsic07_cc, l_jbot, l_jbotpd,
                                l_jbnssec_dv, l_jbnssec3_dv, l_jbnssec5_dv, l_jbnssec8_dv, l_jbsize)
  employees_vars   <- Hmisc::Cs(l_paygl, l_paynl, l_payu, l_payug, l_paytyp, l_ovtpay, l_pvtpyset, l_extrate, l_extrest, l_basnset, l_basrate, l_basrest, l_ovtnset, l_ovtrate, l_ovtrest)
  s.emp_vars       <- Hmisc::Cs(l_jshrs, l_jspayu, l_jspytx, l_jspyni)
  non.emp_vars     <- Hmisc::Cs(l_jbhad)
  job2_vars        <- Hmisc::Cs(l_j2has, l_j2semp, l_j2hrs, l_j2pay)
  benefits_vars    <- Hmisc::Cs(l_benbase1, l_benbase2, l_benbase3, l_benbase4, l_benbase96,
                                l_benctc)
  pension_vars     <- Hmisc::Cs(l_benpen1, l_benpen2, l_benpen3, l_benpen4, l_benpen5, l_benpen6, l_benpen7, l_benpen8, l_benpen96)
  bendis_vars      <- Hmisc::Cs(l_bendis1, l_bendis2, l_bendis3, l_bendis4, l_bendis5, l_bendis12,
                                l_bendis7, l_bendis8, l_bendis10, l_bendis97, l_bendis96)
  otherben_vars    <- Hmisc::Cs(l_benesa,
                                l_othben1, l_othben2, l_othben5, l_othben6, l_othben7, l_othben8, l_othben9, l_othben97, l_othben96)
  benincome_vars   <- Hmisc::Cs(l_bensta2, l_bensta3, l_bensta4, l_bensta5, l_bensta6, l_bensta7, l_bensta97, l_bensta96)
  hhfinance_vars   <- Hmisc::Cs(l_fiyrdia, l_fiyrdb1, l_fiyrdb2, l_fiyrdb3, l_fiyrdb4, l_fiyrdb5, l_fiyrdb6, l_finnow, l_finfut)
  education_vars   <- Hmisc::Cs(l_hiqual_dv)
  health_vars      <- Hmisc::Cs(l_health, l_aidhh, l_sclfsat1, l_sclfsato, l_sf12pcs_dv, l_sf12mcs_dv,
                                l_scsf1, l_scsf2a, l_scsf2b, l_scsf3a, l_scsf3b, l_scsf4a, l_scsf4b, l_scsf5, l_scsf6a,
                                l_scsf6b, l_scsf6c, l_scsf7)
  preg_vars        <- Hmisc::Cs(l_preg,
                                l_pregout1, l_pregend1, l_pregsmoke1, l_smkmnth11, l_smkmnth21, l_smkmnth31, l_pregsmk11, l_pregsmk21, l_pregsmk31, l_aedrof1, l_aepuwk1, l_aepuda1, l_lchmulti1,
                                l_pregout2, l_pregend2, l_pregsmoke2, l_smkmnth12, l_smkmnth22, l_smkmnth32, l_pregsmk12, l_pregsmk22, l_pregsmk32, l_aedrof2, l_aepuwk2, l_aepuda2, l_lchmulti2,
                                l_pregout3, l_pregend3, l_pregsmoke3, l_smkmnth13, l_smkmnth23, l_smkmnth33, l_pregsmk13, l_pregsmk23, l_pregsmk33, l_aedrof3, l_aepuwk3, l_aepuda3, l_lchmulti3,
                                l_nnewborn)
  smoke_vars       <- Hmisc::Cs(l_smoker, l_ncigs, l_giveup, l_gvupreas1, l_gvupreas2, l_gvupreas3, l_gvupreas4, l_gvupreas5, l_gvupreas6, l_gvupreas7, l_gvupreas8, l_gvupreas9, l_ecigs1)
  alc_vars         <- Hmisc::Cs(l_dklm, l_drnk4w, l_evralc, l_fivealcdr, l_auditc1, l_auditc2, l_auditc3, l_auditc4, l_auditc5)
  weight_vars      <- Hmisc::Cs(l_indinus_lw, l_indinui_xw)

  names <- c(id_vars, demographic_vars, prev_wave_vars, econ_stat_vars, work_vars, employees_vars,
             s.emp_vars, non.emp_vars, job2_vars, benefits_vars, pension_vars, bendis_vars, otherben_vars,
             benincome_vars, hhfinance_vars, education_vars, health_vars, preg_vars, smoke_vars, alc_vars, weight_vars)
  names <- tolower(names)

  data <- data[ , names, with = F]

  data.table::setnames(data,

                       c("pidp","pid","l_hidp","l_pno","l_psu","l_strata","l_istrtdaty","l_istrtdatm","l_istrtdatd",
                         ## demographic
                         "l_sex","l_dvage","l_birthy","l_gor_dv","l_urban_dv","l_mlstat","l_marstat",
                         ## previous wave variables
                         "l_notempchk","l_empchk",
                         ## economic status
                         "l_jbstat","l_jbhas","l_jboff","l_jboffy","l_jbterm1","l_jbterm2","l_jbsemp","l_jbpen","l_jbpenm",
                         ## work variables
                         "l_paygu_dv","l_payg_dv","l_jbhrs","l_fimnlabgrs_dv","l_seearngrs_dv","l_jbsic07_cc","l_jbot","l_jbotpd",
                         "l_jbnssec_dv","l_jbnssec3_dv","l_jbnssec5_dv","l_jbnssec8_dv", "l_jbsize",
                         ## employees
                         "l_paygl","l_paynl","l_payu","l_payug","l_paytyp","l_ovtpay","l_pvtpyset","l_extrate","l_extrest","l_basnset","l_basrate",
                         "l_basrest","l_ovtnset","l_ovtrate","l_ovtrest",
                         ## self-employed
                         "l_jshrs","l_jspayu","l_jspytx","l_jspyni",
                         ## non-employed
                         "l_jbhad",
                         ## second job
                         "l_j2has","l_j2semp","l_j2hrs","l_j2pay",
                         ## benefits
                         "l_benbase1","l_benbase2","l_benbase3","l_benbase4","l_benbase96",
                         "l_benctc",
                         ## pensions
                         "l_benpen1","l_benpen2","l_benpen3","l_benpen4","l_benpen5","l_benpen6","l_benpen7","l_benpen8","l_benpen96",
                         ## disability benefits
                         "l_bendis1","l_bendis2","l_bendis3","l_bendis4","l_bendis5","l_bendis12",
                         "l_bendis7","l_bendis8","l_bendis10","l_bendis97","l_bendis96",
                         ## other benefits
                         "l_benesa",
                         "l_othben1","l_othben2","l_othben5","l_othben6","l_othben7","l_othben8","l_othben9","l_othben97","l_othben96",
                         ## benefit income variables (formerly receivables)
                         "l_bensta2","l_bensta3","l_bensta4","l_bensta5","l_bensta6","l_bensta7","l_bensta97","l_bensta96",
                         ## household finance variables (interest and dividends)
                         "l_fiyrdia","l_fiyrdb1","l_fiyrdb2","l_fiyrdb3","l_fiyrdb4","l_fiyrdb5","l_fiyrdb6","l_finnow","l_finfut",
                         ## education variables
                         "l_hiqual_dv",
                         ## health variables
                         "l_health","l_aidhh","l_sclfsat1","l_sclfsato","l_sf12pcs_dv","l_sf12mcs_dv",
                         "l_scsf1","l_scsf2a","l_scsf2b","l_scsf3a","l_scsf3b","l_scsf4a","l_scsf4b","l_scsf5","l_scsf6a","l_scsf6b","l_scsf6c","l_scsf7",
                         ## pregnancy variables
                         "l_preg",
                         "l_pregout1","l_pregend1","l_pregsmoke1","l_smkmnth11","l_smkmnth21","l_smkmnth31","l_pregsmk11","l_pregsmk21","l_pregsmk31","l_aedrof1","l_aepuwk1","l_aepuda1","l_lchmulti1",
                         "l_pregout2","l_pregend2","l_pregsmoke2","l_smkmnth12","l_smkmnth22","l_smkmnth32","l_pregsmk12","l_pregsmk22","l_pregsmk32","l_aedrof2","l_aepuwk2","l_aepuda2","l_lchmulti2",
                         "l_pregout3","l_pregend3","l_pregsmoke3","l_smkmnth13","l_smkmnth23","l_smkmnth33","l_pregsmk13","l_pregsmk23","l_pregsmk33","l_aedrof3","l_aepuwk3","l_aepuda3","l_lchmulti3",
                         "l_nnewborn",
                         ## smoke variables
                         "l_smoker", "l_ncigs", "l_giveup", "l_gvupreas1", "l_gvupreas2", "l_gvupreas3",
                         "l_gvupreas4", "l_gvupreas5", "l_gvupreas6", "l_gvupreas7", "l_gvupreas8", "l_gvupreas9", "l_ecigs1",
                         ## alcohol variables
                         "l_dklm","l_drnk4w","l_evralc","l_fivealcdr",
                         "l_auditc1","l_auditc2","l_auditc3","l_auditc4","l_auditc5",
                         ## weight
                         "l_indinus_lw","l_indinui_xw"),

                       c("pidp","pid","hidp","person_number","psu","strata","year","month","day",
                         ## demographic
                         "sex","age","birth_year","region","urban","mlstat","marstat",
                         ## previous wave variables
                         "notempchk","empchk",
                         ## economic status
                         "econ_stat","jbhas","jboff","jboffy","jbterm1","jbterm2","jbsemp","jbpen","jbpen_member",
                         ## work variables
                         "grss_pay_usual","grss_pay_last","hours","grss_lab_inc","grss_semp","sic07","ovthours_pw","ovthours_paid",
                         "nssec","nssec_3cat","nssec_5cat","nssec_8cat", "jbsize",
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
                         "benbase1","benbase2","benbase3","benbase4","benbase96",
                         "benctc",
                         ## pensions
                         "NI.state_pen","employer_pen","spouse.emp_pen","pencred_pen","prvt_pen","widow_pen","parent_pen","benpen8","non_benpen",
                         ## disability benefits
                         "bendis1","bendis2","bendis3","bendis4","bendis5","bendis12",
                         "bendis7","bendis8","bendis10","bendis97","bendis96",
                         ## other benefits
                         "benesa","othben1","othben2","othben5","othben6",
                         "othben7","othben8","othben9","othben97","othben96",
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
                         "dklm","drnk4w","evralc","fivealcdr",
                         "auditc1","auditc2","auditc3","auditc4","auditc5",
                         ## weight
                         "weight_lw","weight_xw"))

  data[, wave := "UKHLS Wave 12"]
  data[, wave_no := 12]
  data[, bhps_sample := ifelse(!is.na(pid),TRUE,FALSE)]
  data[, dataset := "UKHLS"]
  data[, id := ifelse(bhps_sample==FALSE, pidp, pid)]

  ########################################
  ######## ADD IN HOUSEHOLD DATA #########

  cat(crayon::green("\tHousehold..."))

  data.hhold <- data.table::fread(
    paste0(path, "/l_hhresp.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(data.hhold, names(data.hhold), tolower(names(data.hhold)))

  hhold_vars          <- Hmisc::Cs(l_hidp, l_tenure_dv, l_nkids_dv, l_hhsize, l_hhtype_dv,
                                   l_nch02_dv, l_nch34_dv, l_nch511_dv, l_nch1215_dv,
                                   l_fihhmngrs1_dv, l_fihhmnlabgrs_dv,
                                   l_fihhmnnet1_dv, l_fihhmnlabnet_dv, l_fihhmnsben_dv)

  data.hhold <- data.hhold[ , hhold_vars, with = F]
  data.table::setnames(data.hhold,
                       # old names
                       c("l_hidp", "l_tenure_dv", "l_nkids_dv", "l_hhsize","l_hhtype_dv",
                         "l_nch02_dv", "l_nch34_dv", "l_nch511_dv", "l_nch1215_dv",
                         "l_fihhmngrs1_dv", "l_fihhmnlabgrs_dv",
                         "l_fihhmnnet1_dv", "l_fihhmnlabnet_dv", "l_fihhmnsben_dv"),
                       # new names
                       c("hidp", "hh_tenure", "hh_numchild", "hh_size", "hh_type",
                         "hh_numchild02", "hh_numchild34", "hh_numchild511", "hh_numchild1215",
                         "hh_fihhmngrs1_dv", "hh_fihhmnlabgrs_dv",
                         "hh_fihhmnnet1_dv", "hh_fihhmnlabnet_dv", "hh_fihhmnsben_dv"))

  hhold_merged <- merge(x = data,
                        y = data.hhold,
                        by="hidp",
                        all.x=TRUE,
                        all.y=FALSE)

  #########################################
  ######## ADD IN CROSS-WAVE DATA #########

  cat(crayon::green("\tCross-Wave..."))

  data.xwave <- data.table::fread(
    paste0(path, "/xwavedat.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(data.xwave, names(data.xwave), tolower(names(data.xwave)))

  xwave_vars  <- colnames(data.xwave[ , c("pidp","ethn_dv","dcsedfl_dv","dcsedw_dv")])

  data.xwave <- data.xwave[ , xwave_vars, with = F]
  data.table::setnames(data.xwave,
                       # old names
                       c("pidp","ethn_dv","dcsedfl_dv","dcsedw_dv"),
                       # new names
                       c("pidp","ethnicity_raw","deceased","deceased_when"))

  ####### Combine - keep all observations in the main data and drop excess xwave observations

  xwave_merged <- merge(x = hhold_merged,
                        y = data.xwave,
                                   by="pidp",
                                   all.x=TRUE,
                                   all.y=FALSE)

  ####################################################
  #### ADD IN THE INDALL DATA ########################

  cat(crayon::green("\tIndall..."))

  data.indall <- data.table::fread(
    paste0(path, "/l_indall.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(data.indall, names(data.indall), tolower(names(data.indall)))

  indall_vars  <- colnames(data.indall[ , c("pidp","l_hidp",
                                           "l_imd2019qe_dv","l_imd2017qni_dv",
                                           "l_imd2020qs_dv","l_imd2019qw_dv")])

  data.indall <- data.indall[ , indall_vars, with = F]
  data.table::setnames(data.indall,
                       # old names
                       c("pidp","l_hidp",
                         "l_imd2019qe_dv","l_imd2017qni_dv",
                         "l_imd2020qs_dv","l_imd2019qw_dv"),
                       # new names
                       c("pidp","hidp",
                         "imdq_e","imdq_ni",
                         "imdq_s","imdq_w"))

  ## Combine - keep all observations in the main data and drop excess xwave observations

  data_merged <- merge(x = xwave_merged,
                       y = data.indall,
                       by = c("pidp","hidp"),
                       all.x = TRUE,
                       all.y = FALSE)

  ##########################################################################

  cat(crayon::blue(crayon::bold("\tdone\n")))

  return(data_merged)
}
