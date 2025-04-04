#' Read Understanding Society Wave 14
#'
#' Reads and performs basic cleaning operations on the UKHLS 14th wave (Jan 2022 - May 2024). Missing values as detailed below are all set to NA.
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
ukhls_read_wave14 <- function(
    root = c("X:/"),
    file = "HAR_PR/PR/USoc/Data/SN6614_2024_12_02/tab/ukhls/",
    full = TRUE
) {

  cat(crayon::blue(crayon::underline("\tReading UKHLS Wave 14 datasets")))

  cat(crayon::green("\tIndividual..."))

  path <- here::here(paste0(root, file))

  data <- data.table::fread(
    paste0(path, "/n_indresp.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-10", "-90", "-90.0", "N/A")
  )
  if (full == TRUE) {
    # retain full interviews only
    data <- data[n_ivfio==1,]
  }

  data.table::setnames(data, names(data), tolower(names(data)))

  id_vars          <- Hmisc::Cs(pidp, pid, n_hidp, n_pno, n_psu, n_strata, n_istrtdaty, n_istrtdatm, n_istrtdatd)
  demographic_vars <- Hmisc::Cs(n_sex, n_dvage, n_birthy, n_gor_dv, n_urban_dv, n_mlstat, n_marstat)
  prev_wave_vars   <- Hmisc::Cs(n_notempchk, n_empchk)
  econ_stat_vars   <- Hmisc::Cs(n_jbstat, n_jbhas, n_jboff, n_jboffy, n_jbterm1, n_jbterm2, n_jbsemp)
  work_vars        <- Hmisc::Cs(n_paygu_dv, n_payg_dv, n_jbhrs, n_fimnlabgrs_dv, n_seearngrs_dv, n_jbsic07_cc, n_jbot, n_jbotpd,
                                n_jbnssec_dv, n_jbnssec3_dv, n_jbnssec5_dv, n_jbnssec8_dv, n_jbsize)
  employees_vars   <- Hmisc::Cs(n_paygl, n_paynl, n_payu, n_payug, n_paytyp, n_ovtpay, n_pvtpyset, n_extrate, n_basnset, n_basrate, n_ovtnset, n_ovtrate)
  s.emp_vars       <- Hmisc::Cs(n_jshrs, n_jspayu, n_jspytx, n_jspyni)
  non.emp_vars     <- Hmisc::Cs(n_jbhad)
  job2_vars        <- NULL #Hmisc::Cs(n_j2has, n_j2semp, n_j2hrs, n_j2pay)
  benefits_vars    <- Hmisc::Cs(n_benbase1, n_benbase2, n_benbase3, n_benbase4, n_benbase96,
                                n_benctc)
  pension_vars     <- Hmisc::Cs(n_benpen1, n_benpen2, n_benpen3, n_benpen4, n_benpen5, n_benpen6, n_benpen7, n_benpen8, n_benpen96)
  bendis_vars      <- Hmisc::Cs(n_bendis1, n_bendis2, n_bendis3, n_bendis4, n_bendis5, n_bendis12,
                                n_bendis7, n_bendis8, n_bendis10, n_bendis97, n_bendis96)
  otherben_vars    <- Hmisc::Cs(n_benesa,
                                n_othben1, n_othben2, n_othben5, n_othben6, n_othben7, n_othben8, n_othben9, n_othben97, n_othben96)
  benincome_vars   <- Hmisc::Cs(n_bensta2, n_bensta3, n_bensta4, n_bensta5, n_bensta6, n_bensta7, n_bensta97, n_bensta96)
  hhfinance_vars   <- Hmisc::Cs(n_fiyrdia, n_fiyrdb1, n_fiyrdb2, n_fiyrdb3, n_fiyrdb4, n_fiyrdb5, n_fiyrdb6, n_finnow, n_finfut)
  education_vars   <- Hmisc::Cs(n_hiqual_dv)
  health_vars      <- Hmisc::Cs(n_health, n_aidhh, n_sclfsat1, n_sclfsato, n_sf12pcs_dv, n_sf12mcs_dv,
                                n_scsf1, n_scsf2a, n_scsf2b, n_scsf3a, n_scsf3b, n_scsf4a, n_scsf4b, n_scsf5, n_scsf6a,
                                n_scsf6b, n_scsf6c, n_scsf7)
  health_cond_vars <- Hmisc::Cs(n_hconds01, n_hconds03, n_hconds04, n_hconds05, n_hconds08,
                                            n_hconds11, n_hconds12, n_hconds15, n_hconds16,
                                n_hconds21,                                     n_hconds26, n_hconds27, n_hconds28, n_hconds29,
                                n_hconds30, n_hconds31, n_hconds32, n_hconds33, n_hconds34, n_hconds35,
                                n_hconds38, n_hconds39,
                                n_hconds40, n_hconds41, n_hconds42,
                                n_hconds66, n_hconds67, n_hconds68, n_hconds69, n_hconds72, n_hconds73, n_hconds74, n_hconds75,
                                n_hconds79, n_hconds80, n_hconds81, n_hconds82, n_hconds86, n_hconds88, n_hconds89,

                                n_hcondns1, n_hcondns3, n_hcondns4, n_hcondns5, n_hcondns6, n_hcondns7, n_hcondns8,
                                n_hcondns10, n_hcondns11, n_hcondns12, n_hcondns15, n_hcondns16, n_hcondns19,
                                n_hcondns21, n_hcondns23, n_hcondns24, n_hcondns26, n_hcondns27, n_hcondns28, n_hcondns29,
                                n_hcondns30, n_hcondns31, n_hcondns33, n_hcondns34, n_hcondns35,              n_hcondns38, n_hcondns39,
                                n_hcondns40, n_hcondns41, n_hcondns42,
                                n_hcondns66, n_hcondns67, n_hcondns68, n_hcondns69, n_hcondns70, n_hcondns71, n_hcondns72, n_hcondns73, n_hcondns74, n_hcondns75,
                                n_hcondns78, n_hcondns79, n_hcondns80, n_hcondns81, n_hcondns82, n_hcondns83, n_hcondns84, n_hcondns86, n_hcondns87, n_hcondns88, n_hcondns89)
  preg_vars        <- Hmisc::Cs(n_preg,
                                n_pregout1, n_pregend1, n_pregsmoke1, n_smkmnth11, n_smkmnth21, n_smkmnth31, n_pregsmk11, n_pregsmk21, n_pregsmk31, n_aedrof1, n_aepuwk1, n_aepuda1, n_lchmulti1,
                                n_pregout2, n_pregend2, n_pregsmoke2, n_smkmnth12, n_smkmnth22, n_smkmnth32, n_pregsmk12, n_pregsmk22, n_pregsmk32, n_aedrof2, n_aepuwk2, n_aepuda2, n_lchmulti2,
                                n_pregout3, n_pregend3, n_pregsmoke3, n_smkmnth13, n_smkmnth23, n_smkmnth33, n_pregsmk13, n_pregsmk23, n_pregsmk33, n_aedrof3, n_aepuwk3, n_aepuda3, n_lchmulti3,
                                n_nnewborn)
  smoke_vars       <- Hmisc::Cs(n_smoker, n_ncigs, n_giveup, n_gvupreas1, n_gvupreas2, n_gvupreas3, n_gvupreas4, n_gvupreas5, n_gvupreas6, n_gvupreas7, n_gvupreas8, n_gvupreas9, n_ecigs1)
  alc_vars         <- NULL #Hmisc::Cs(n_auditc1, n_auditc2, n_auditc3, n_auditc4, n_auditc5)
  weight_vars      <- Hmisc::Cs(n_indinus_lw, n_inding2_xw)

  names <- c(id_vars, demographic_vars, prev_wave_vars, econ_stat_vars, work_vars, employees_vars,
             s.emp_vars, non.emp_vars, job2_vars, benefits_vars, pension_vars, bendis_vars, otherben_vars,
             benincome_vars, hhfinance_vars, education_vars, health_vars, health_cond_vars, preg_vars, smoke_vars, alc_vars, weight_vars)
  names <- tolower(names)

  data <- data[ , names, with = F]

  data.table::setnames(data,

                       c("pidp","pid","n_hidp","n_pno","n_psu","n_strata","n_istrtdaty","n_istrtdatm","n_istrtdatd",
                         ## demographic
                         "n_sex","n_dvage","n_birthy","n_gor_dv","n_urban_dv","n_mlstat","n_marstat",
                         ## previous wave variables
                         "n_notempchk","n_empchk",
                         ## economic status
                         "n_jbstat","n_jbhas","n_jboff","n_jboffy","n_jbterm1","n_jbterm2","n_jbsemp",
                         ## work variables
                         "n_paygu_dv","n_payg_dv","n_jbhrs","n_fimnlabgrs_dv","n_seearngrs_dv","n_jbsic07_cc","n_jbot","n_jbotpd",
                         "n_jbnssec_dv","n_jbnssec3_dv","n_jbnssec5_dv","n_jbnssec8_dv", "n_jbsize",
                         ## employees
                         "n_paygl","n_paynl","n_payu","n_payug","n_paytyp","n_ovtpay","n_pvtpyset","n_extrate","n_basnset","n_basrate",
                         "n_ovtnset","n_ovtrate",
                         ## self-employed
                         "n_jshrs","n_jspayu","n_jspytx","n_jspyni",
                         ## non-employed
                         "n_jbhad",
                         ## second job

                         ## benefits
                         "n_benbase1","n_benbase2","n_benbase3","n_benbase4","n_benbase96",
                         "n_benctc",
                         ## pensions
                         "n_benpen1","n_benpen2","n_benpen3","n_benpen4","n_benpen5","n_benpen6","n_benpen7","n_benpen8","n_benpen96",
                         ## disability benefits
                         "n_bendis1","n_bendis2","n_bendis3","n_bendis4","n_bendis5","n_bendis12",
                         "n_bendis7","n_bendis8","n_bendis10","n_bendis97","n_bendis96",
                         ## other benefits
                         "n_benesa",
                         "n_othben1","n_othben2","n_othben5","n_othben6","n_othben7","n_othben8","n_othben9","n_othben97","n_othben96",
                         ## benefit income variables (formerly receivables)
                         "n_bensta2","n_bensta3","n_bensta4","n_bensta5","n_bensta6","n_bensta7","n_bensta97","n_bensta96",
                         ## household finance variables (interest and dividends)
                         "n_fiyrdia","n_fiyrdb1","n_fiyrdb2","n_fiyrdb3","n_fiyrdb4","n_fiyrdb5","n_fiyrdb6","n_finnow","n_finfut",
                         ## education variables
                         "n_hiqual_dv",
                         ## health variables
                         "n_health","n_aidhh","n_sclfsat1","n_sclfsato","n_sf12pcs_dv","n_sf12mcs_dv",
                         "n_scsf1","n_scsf2a","n_scsf2b","n_scsf3a","n_scsf3b","n_scsf4a","n_scsf4b","n_scsf5","n_scsf6a","n_scsf6b","n_scsf6c","n_scsf7",
                         ### health conditions
                         "n_hconds01", "n_hconds03", "n_hconds04", "n_hconds05", "n_hconds08",
                         "n_hconds11", "n_hconds12", "n_hconds15", "n_hconds16",
                         "n_hconds21",                                     "n_hconds26", "n_hconds27", "n_hconds28", "n_hconds29",
                         "n_hconds30", "n_hconds31", "n_hconds32", "n_hconds33", "n_hconds34", "n_hconds35",
                         "n_hconds38", "n_hconds39",
                         "n_hconds40", "n_hconds41", "n_hconds42",
                         "n_hconds66", "n_hconds67", "n_hconds68", "n_hconds69", "n_hconds72", "n_hconds73", "n_hconds74", "n_hconds75",
                         "n_hconds79", "n_hconds80", "n_hconds81", "n_hconds82", "n_hconds86", "n_hconds88", "n_hconds89",

                         "n_hcondns1", "n_hcondns3", "n_hcondns4", "n_hcondns5", "n_hcondns6", "n_hcondns7", "n_hcondns8",
                         "n_hcondns10", "n_hcondns11", "n_hcondns12", "n_hcondns15", "n_hcondns16", "n_hcondns19",
                         "n_hcondns21", "n_hcondns23", "n_hcondns24", "n_hcondns26", "n_hcondns27", "n_hcondns28", "n_hcondns29",
                         "n_hcondns30", "n_hcondns31", "n_hcondns33", "n_hcondns34", "n_hcondns35",              "n_hcondns38", "n_hcondns39",
                         "n_hcondns40", "n_hcondns41", "n_hcondns42",
                         "n_hcondns66", "n_hcondns67", "n_hcondns68", "n_hcondns69", "n_hcondns70", "n_hcondns71", "n_hcondns72", "n_hcondns73", "n_hcondns74", "n_hcondns75",
                         "n_hcondns78", "n_hcondns79", "n_hcondns80", "n_hcondns81", "n_hcondns82", "n_hcondns83", "n_hcondns84", "n_hcondns86", "n_hcondns87","n_hcondns88", "n_hcondns89",

                         ## pregnancy variables
                         "n_preg",
                         "n_pregout1","n_pregend1","n_pregsmoke1","n_smkmnth11","n_smkmnth21","n_smkmnth31","n_pregsmk11","n_pregsmk21","n_pregsmk31","n_aedrof1","n_aepuwk1","n_aepuda1","n_lchmulti1",
                         "n_pregout2","n_pregend2","n_pregsmoke2","n_smkmnth12","n_smkmnth22","n_smkmnth32","n_pregsmk12","n_pregsmk22","n_pregsmk32","n_aedrof2","n_aepuwk2","n_aepuda2","n_lchmulti2",
                         "n_pregout3","n_pregend3","n_pregsmoke3","n_smkmnth13","n_smkmnth23","n_smkmnth33","n_pregsmk13","n_pregsmk23","n_pregsmk33","n_aedrof3","n_aepuwk3","n_aepuda3","n_lchmulti3",
                         "n_nnewborn",
                         ## smoke variables
                         "n_smoker", "n_ncigs", "n_giveup", "n_gvupreas1", "n_gvupreas2", "n_gvupreas3",
                         "n_gvupreas4", "n_gvupreas5", "n_gvupreas6", "n_gvupreas7", "n_gvupreas8", "n_gvupreas9", "n_ecigs1",
                         ## alcohol variables
                         #"n_auditc1","n_auditc2","n_auditc3","n_auditc4","n_auditc5",
                         ## weight
                         "n_indinus_lw","n_inding2_xw"),

                       c("pidp","pid","hidp","person_number","psu","strata","year","month","day",
                         ## demographic
                         "sex","age","birth_year","region","urban","mlstat","marstat",
                         ## previous wave variables
                         "notempchk","empchk",
                         ## economic status
                         "econ_stat","jbhas","jboff","jboffy","jbterm1","jbterm2","jbsemp",
                         ## work variables
                         "grss_pay_usual","grss_pay_last","hours","grss_lab_inc","grss_semp","sic07","ovthours_pw","ovthours_paid",
                         "nssec","nssec_3cat","nssec_5cat","nssec_8cat", "jbsize",
                         ## employees
                         "last_gross_pay","last_net_pay","usuam_pay","payug","pay_type","ovtpay","additional.pay_set","extrate","basic.pay_set","baspay_rate",
                         "ovt.pay_set","ovtpay_rate",
                         ## self-employed
                         "s.emp_hours","s.emp_pay","s.emp_pay_pretax","s.emp_pay_preNI",
                         ## non-employed
                         "jbhad",
                         ## second job

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
                         ### health conditions
                         "hconds01", "hconds03", "hconds04", "hconds05", "hconds08",
                         "hconds11", "hconds12", "hconds15", "hconds16",
                         "hconds21",                                     "hconds26", "hconds27", "hconds28", "hconds29",
                         "hconds30", "hconds31", "hconds32", "hconds33", "hconds34", "hconds35",
                         "hconds38", "hconds39",
                         "hconds40", "hconds41", "hconds42",
                         "hconds66", "hconds67", "hconds68", "hconds69", "hconds72", "hconds73", "hconds74", "hconds75",
                         "hconds79", "hconds80", "hconds81", "hconds82", "hconds86", "hconds88", "hconds89",

                         "hcondns1", "hcondns3", "hcondns4", "hcondns5", "hcondns6", "hcondns7", "hcondns8",
                         "hcondns10", "hcondns11", "hcondns12", "hcondns15", "hcondns16", "hcondns19",
                         "hcondns21", "hcondns23", "hcondns24", "hcondns26", "hcondns27", "hcondns28", "hcondns29",
                         "hcondns30", "hcondns31", "hcondns33", "hcondns34", "hcondns35",              "hcondns38", "hcondns39",
                         "hcondns40", "hcondns41", "hcondns42",
                         "hcondns66", "hcondns67", "hcondns68", "hcondns69", "hcondns70", "hcondns71", "hcondns72", "hcondns73", "hcondns74", "hcondns75",
                         "hcondns78", "hcondns79", "hcondns80", "hcondns81", "hcondns82", "hcondns83", "hcondns84", "hcondns86", "hcondns87","hcondns88", "hcondns89",
                         ## pregnancy variables
                         "preg",
                         "pregout1","pregend1","pregsmoke1","smkmnth11","smkmnth21","smkmnth31","pregsmk_ncigs11","pregsmk_ncigs21","pregsmk_ncigs31","pregdrnk_freq1","pregdrnk_unitpw1","pregdrnk_unit1","lchmulti1",
                         "pregout2","pregend2","pregsmoke2","smkmnth12","smkmnth22","smkmnth32","pregsmk_ncigs12","pregsmk_ncigs22","pregsmk_ncigs32","pregdrnk_freq2","pregdrnk_unitpw2","pregdrnk_unit2","lchmulti2",
                         "pregout3","pregend3","pregsmoke3","smkmnth13","smkmnth23","smkmnth33","pregsmk_ncigs13","pregsmk_ncigs23","pregsmk_ncigs33","pregdrnk_freq3","pregdrnk_unitpw3","pregdrnk_unit3","lchmulti3",
                         "nnewborn",
                         ## smoke variables
                         "smoker", "ncigs", "giveup_smk", "gvupsmk_curr_health_prblm", "gvupsmk_generam_better_health", "gvupsmk_risk_down_smk_illness",
                         "gvupsmk_public_work_smk_ban", "gvupsmk_fam&frnds", "gvupsmk_finances", "gvupsmk_chld_effect", "gvupsmk_fam_effect", "gvupsmk_other_reason", "ecigs",
                         ## alcohol variables
                         #"auditc1","auditc2","auditc3","auditc4","auditc5",
                         ## weight
                         "weight_lw","weight_xw"))

  data[, wave := "UKHLS Wave 14"]
  data[, wave_no := 14]
  data[, bhps_sample := ifelse(!is.na(pid),TRUE,FALSE)]
  data[, dataset := "UKHLS"]
  data[, id := ifelse(bhps_sample==FALSE, pidp, pid)]

  ########################################
  ######## ADD IN HOUSEHOLD DATA #########

  cat(crayon::green("\tHousehold..."))

  data.hhold <- data.table::fread(
    paste0(path, "/n_hhresp.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(data.hhold, names(data.hhold), tolower(names(data.hhold)))

  hhold_vars          <- Hmisc::Cs(n_hidp, n_tenure_dv, n_nkids_dv, n_hhsize, n_hhtype_dv,
                                   n_nch02_dv, n_nch34_dv, n_nch511_dv, n_nch1215_dv,
                                   n_fihhmngrs1_dv, n_fihhmnlabgrs_dv,
                                   n_fihhmnnet1_dv, n_fihhmnlabnet_dv, n_fihhmnsben_dv)

  data.hhold <- data.hhold[ , hhold_vars, with = F]
  data.table::setnames(data.hhold,
                       # old names
                       c("n_hidp", "n_tenure_dv", "n_nkids_dv", "n_hhsize","n_hhtype_dv",
                         "n_nch02_dv", "n_nch34_dv", "n_nch511_dv", "n_nch1215_dv",
                         "n_fihhmngrs1_dv", "n_fihhmnlabgrs_dv",
                         "n_fihhmnnet1_dv", "n_fihhmnlabnet_dv", "n_fihhmnsben_dv"),
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
    paste0(path, "/n_indall.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(data.indall, names(data.indall), tolower(names(data.indall)))

  indalm_vars  <- colnames(data.indall[ , c("pidp","n_hidp",
                                            "n_imd2019qe_dv","n_imd2017qni_dv",
                                            "n_imd2020qs_dv","n_imd2019qw_dv")])

  data.indall <- data.indall[ , indalm_vars, with = F]
  data.table::setnames(data.indall,
                       # old names
                       c("pidp","n_hidp",
                         "n_imd2019qe_dv","n_imd2017qni_dv",
                         "n_imd2020qs_dv","n_imd2019qw_dv"),
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
