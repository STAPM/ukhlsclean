#' Read Understanding Society Wave 13
#'
#' Reads and performs basic cleaning operations on the UKHLS 13th wave (Jan 2021 - May 2023). Missing values as detailed below are all set to NA.
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
ukhls_read_wave13 <- function(
    root = c("X:/"),
    file = "HAR_PR/PR/USoc/Data/SN6614_2024_12_02/tab/ukhls/",
    full = TRUE
) {

  cat(crayon::blue(crayon::underline("\tReading UKHLS Wave 13 datasets")))

  cat(crayon::green("\tIndividual..."))

  path <- here::here(paste0(root, file))

  data <- data.table::fread(
    paste0(path, "/m_indresp.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-10", "-90", "-90.0", "N/A")
  )
  if (full == TRUE) {
    # retain full interviews only
    data <- data[m_ivfio==1,]
  }

  data.table::setnames(data, names(data), tolower(names(data)))

  id_vars          <- Hmisc::Cs(pidp, pid, m_hidp, m_pno, m_psu, m_strata, m_istrtdaty, m_istrtdatm, m_istrtdatd)
  demographic_vars <- Hmisc::Cs(m_sex, m_dvage, m_birthy, m_gor_dv, m_urban_dv, m_mlstat, m_marstat)
  prev_wave_vars   <- Hmisc::Cs(m_notempchk, m_empchk)
  econ_stat_vars   <- Hmisc::Cs(m_jbstat, m_jbhas, m_jboff, m_jboffy, m_jbterm1, m_jbterm2, m_jbsemp)
  work_vars        <- Hmisc::Cs(m_paygu_dv, m_payg_dv, m_jbhrs, m_fimnlabgrs_dv, m_seearngrs_dv, m_jbsic07_cc, m_jbot, m_jbotpd,
                                m_jbnssec_dv, m_jbnssec3_dv, m_jbnssec5_dv, m_jbnssec8_dv, m_jbsize)
  employees_vars   <- Hmisc::Cs(m_paygl, m_paynl, m_payu, m_payug, m_paytyp, m_ovtpay, m_pvtpyset, m_extrate, m_basnset, m_basrate, m_ovtnset, m_ovtrate)
  s.emp_vars       <- Hmisc::Cs(m_jshrs, m_jspayu, m_jspytx, m_jspyni)
  non.emp_vars     <- Hmisc::Cs(m_jbhad)
  job2_vars        <- NULL #Hmisc::Cs(m_j2has, m_j2semp, m_j2hrs, m_j2pay)
  benefits_vars    <- Hmisc::Cs(m_benbase1, m_benbase2, m_benbase3, m_benbase4, m_benbase96,
                                m_benctc)
  pension_vars     <- Hmisc::Cs(m_benpen1, m_benpen2, m_benpen3, m_benpen4, m_benpen5, m_benpen6, m_benpen7, m_benpen8, m_benpen96)
  bendis_vars      <- Hmisc::Cs(m_bendis1, m_bendis2, m_bendis3, m_bendis4, m_bendis5, m_bendis12,
                                m_bendis7, m_bendis8, m_bendis10, m_bendis97, m_bendis96)
  otherben_vars    <- Hmisc::Cs(m_benesa,
                                m_othben1, m_othben2, m_othben5, m_othben6, m_othben7, m_othben8, m_othben9, m_othben97, m_othben96)
  benincome_vars   <- Hmisc::Cs(m_bensta2, m_bensta3, m_bensta4, m_bensta5, m_bensta6, m_bensta7, m_bensta97, m_bensta96)
  hhfinance_vars   <- Hmisc::Cs(m_fiyrdia, m_fiyrdb1, m_fiyrdb2, m_fiyrdb3, m_fiyrdb4, m_fiyrdb5, m_fiyrdb6, m_finnow, m_finfut)
  education_vars   <- Hmisc::Cs(m_hiqual_dv)
  health_vars      <- Hmisc::Cs(m_health, m_aidhh, m_sclfsat1, m_sclfsato, m_sf12pcs_dv, m_sf12mcs_dv,
                                m_scsf1, m_scsf2a, m_scsf2b, m_scsf3a, m_scsf3b, m_scsf4a, m_scsf4b, m_scsf5, m_scsf6a,
                                m_scsf6b, m_scsf6c, m_scsf7)
  health_cond_vars <- Hmisc::Cs(m_hconds01, m_hconds03, m_hconds04, m_hconds05, m_hconds08,
                                m_hconds10, m_hconds11, m_hconds12, m_hconds15, m_hconds16,
                                m_hconds21, m_hconds23, m_hconds24, m_hconds25, m_hconds26, m_hconds27, m_hconds28, m_hconds29,
                                m_hconds30, m_hconds31, m_hconds32, m_hconds33, m_hconds34, m_hconds35, m_hconds36,
                                m_mhconds38, m_mhconds39,
                                m_mhconds40, m_mhconds41, m_mhconds42,
                                m_mhconds54, m_mhconds55, m_mhconds56, m_mhconds57, m_mhconds59, m_mhconds60, m_mhconds61, m_mhconds62, m_mhconds63,

                                m_hcondns1, m_hcondns3, m_hcondns4, m_hcondns5, m_hcondns6, m_hcondns7, m_hcondns8,
                                m_hcondns10, m_hcondns11, m_hcondns12, m_hcondns15, m_hcondns16, m_hcondns19,
                                m_hcondns21, m_hcondns23, m_hcondns24, m_hcondns26, m_hcondns27, m_hcondns28, m_hcondns29,
                                m_hcondns30, m_hcondns31, m_hcondns33, m_hcondns34, m_hcondns35, m_hcondns37, m_hcondns38, m_hcondns39,
                                m_hcondns40, m_hcondns41, m_hcondns42)
  preg_vars        <- Hmisc::Cs(m_preg,
                                m_pregout1, m_pregend1, m_pregsmoke1, m_smkmnth11, m_smkmnth21, m_smkmnth31, m_pregsmk11, m_pregsmk21, m_pregsmk31, m_aedrof1, m_aepuwk1, m_aepuda1, m_lchmulti1,
                                m_pregout2, m_pregend2, m_pregsmoke2, m_smkmnth12, m_smkmnth22, m_smkmnth32, m_pregsmk12, m_pregsmk22, m_pregsmk32, m_aedrof2, m_aepuwk2, m_aepuda2, m_lchmulti2,
                                m_pregout3, m_pregend3, m_pregsmoke3, m_smkmnth13, m_smkmnth23, m_smkmnth33, m_pregsmk13, m_pregsmk23, m_pregsmk33, m_aedrof3, m_aepuwk3, m_aepuda3, m_lchmulti3,
                                m_nnewborn)
  smoke_vars       <- Hmisc::Cs(m_smoker, m_ncigs, m_giveup, m_gvupreas1, m_gvupreas2, m_gvupreas3, m_gvupreas4, m_gvupreas5, m_gvupreas6, m_gvupreas7, m_gvupreas8, m_gvupreas9, m_ecigs1)
  alc_vars         <- Hmisc::Cs(m_auditc1, m_auditc2, m_auditc3, m_auditc4, m_auditc5)
  weight_vars      <- Hmisc::Cs(m_indinus_lw, m_indinui_xw)

  names <- c(id_vars, demographic_vars, prev_wave_vars, econ_stat_vars, work_vars, employees_vars,
             s.emp_vars, non.emp_vars, job2_vars, benefits_vars, pension_vars, bendis_vars, otherben_vars,
             benincome_vars, hhfinance_vars, education_vars, health_vars, health_cond_vars, preg_vars, smoke_vars, alc_vars, weight_vars)
  names <- tolower(names)

  data <- data[ , names, with = F]

  data.table::setnames(data,

                       c("pidp","pid","m_hidp","m_pno","m_psu","m_strata","m_istrtdaty","m_istrtdatm","m_istrtdatd",
                         ## demographic
                         "m_sex","m_dvage","m_birthy","m_gor_dv","m_urban_dv","m_mlstat","m_marstat",
                         ## previous wave variables
                         "m_notempchk","m_empchk",
                         ## economic status
                         "m_jbstat","m_jbhas","m_jboff","m_jboffy","m_jbterm1","m_jbterm2","m_jbsemp",
                         ## work variables
                         "m_paygu_dv","m_payg_dv","m_jbhrs","m_fimnlabgrs_dv","m_seearngrs_dv","m_jbsic07_cc","m_jbot","m_jbotpd",
                         "m_jbnssec_dv","m_jbnssec3_dv","m_jbnssec5_dv","m_jbnssec8_dv", "m_jbsize",
                         ## employees
                         "m_paygl","m_paynl","m_payu","m_payug","m_paytyp","m_ovtpay","m_pvtpyset","m_extrate","m_basnset","m_basrate",
                         "m_ovtnset","m_ovtrate",
                         ## self-employed
                         "m_jshrs","m_jspayu","m_jspytx","m_jspyni",
                         ## non-employed
                         "m_jbhad",
                         ## second job

                         ## benefits
                         "m_benbase1","m_benbase2","m_benbase3","m_benbase4","m_benbase96",
                         "m_benctc",
                         ## pensions
                         "m_benpen1","m_benpen2","m_benpen3","m_benpen4","m_benpen5","m_benpen6","m_benpen7","m_benpen8","m_benpen96",
                         ## disability benefits
                         "m_bendis1","m_bendis2","m_bendis3","m_bendis4","m_bendis5","m_bendis12",
                         "m_bendis7","m_bendis8","m_bendis10","m_bendis97","m_bendis96",
                         ## other benefits
                         "m_benesa",
                         "m_othben1","m_othben2","m_othben5","m_othben6","m_othben7","m_othben8","m_othben9","m_othben97","m_othben96",
                         ## benefit income variables (formerly receivables)
                         "m_bensta2","m_bensta3","m_bensta4","m_bensta5","m_bensta6","m_bensta7","m_bensta97","m_bensta96",
                         ## household finance variables (interest and dividends)
                         "m_fiyrdia","m_fiyrdb1","m_fiyrdb2","m_fiyrdb3","m_fiyrdb4","m_fiyrdb5","m_fiyrdb6","m_finnow","m_finfut",
                         ## education variables
                         "m_hiqual_dv",
                         ## health variables
                         "m_health","m_aidhh","m_sclfsat1","m_sclfsato","m_sf12pcs_dv","m_sf12mcs_dv",
                         "m_scsf1","m_scsf2a","m_scsf2b","m_scsf3a","m_scsf3b","m_scsf4a","m_scsf4b","m_scsf5","m_scsf6a","m_scsf6b","m_scsf6c","m_scsf7",
                         ## health conditions
                         "m_hconds01", "m_hconds03", "m_hconds04", "m_hconds05", "m_hconds08",
                         "m_hconds10", "m_hconds11", "m_hconds12", "m_hconds15", "m_hconds16",
                         "m_hconds21", "m_hconds23", "m_hconds24", "m_hconds25", "m_hconds26", "m_hconds27", "m_hconds28", "m_hconds29",
                         "m_hconds30", "m_hconds31", "m_hconds32", "m_hconds33", "m_hconds34", "m_hconds35", "m_hconds36",
                         "m_mhconds38", "m_mhconds39",
                         "m_mhconds40", "m_mhconds41", "m_mhconds42",
                         "m_mhconds54", "m_mhconds55", "m_mhconds56", "m_mhconds57", "m_mhconds59", "m_mhconds60", "m_mhconds61", "m_mhconds62", "m_mhconds63",

                         "m_hcondns1", "m_hcondns3", "m_hcondns4", "m_hcondns5", "m_hcondns6", "m_hcondns7", "m_hcondns8",
                         "m_hcondns10", "m_hcondns11", "m_hcondns12", "m_hcondns15", "m_hcondns16", "m_hcondns19",
                         "m_hcondns21", "m_hcondns23", "m_hcondns24", "m_hcondns26", "m_hcondns27", "m_hcondns28", "m_hcondns29",
                         "m_hcondns30", "m_hcondns31", "m_hcondns33", "m_hcondns34", "m_hcondns35", "m_hcondns37", "m_hcondns38", "m_hcondns39",
                         "m_hcondns40", "m_hcondns41", "m_hcondns42",
                         ## pregnancy variables
                         "m_preg",
                         "m_pregout1","m_pregend1","m_pregsmoke1","m_smkmnth11","m_smkmnth21","m_smkmnth31","m_pregsmk11","m_pregsmk21","m_pregsmk31","m_aedrof1","m_aepuwk1","m_aepuda1","m_lchmulti1",
                         "m_pregout2","m_pregend2","m_pregsmoke2","m_smkmnth12","m_smkmnth22","m_smkmnth32","m_pregsmk12","m_pregsmk22","m_pregsmk32","m_aedrof2","m_aepuwk2","m_aepuda2","m_lchmulti2",
                         "m_pregout3","m_pregend3","m_pregsmoke3","m_smkmnth13","m_smkmnth23","m_smkmnth33","m_pregsmk13","m_pregsmk23","m_pregsmk33","m_aedrof3","m_aepuwk3","m_aepuda3","m_lchmulti3",
                         "m_nnewborn",
                         ## smoke variables
                         "m_smoker", "m_ncigs", "m_giveup", "m_gvupreas1", "m_gvupreas2", "m_gvupreas3",
                         "m_gvupreas4", "m_gvupreas5", "m_gvupreas6", "m_gvupreas7", "m_gvupreas8", "m_gvupreas9", "m_ecigs1",
                         ## alcohol variables
                         "m_auditc1","m_auditc2","m_auditc3","m_auditc4","m_auditc5",
                         ## weight
                         "m_indinus_lw","m_indinui_xw"),

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
                         ## health conditions
                         "hconds01", "hconds03", "hconds04", "hconds05", "hconds08",
                         "hconds10", "hconds11", "hconds12", "hconds15", "hconds16",
                         "hconds21", "hconds23", "hconds24", "hconds25", "hconds26", "hconds27", "hconds28", "hconds29",
                         "hconds30", "hconds31", "hconds32", "hconds33", "hconds34", "hconds35", "hconds36",
                         "mhconds38", "mhconds39",
                         "mhconds40", "mhconds41", "mhconds42",
                         "mhconds54", "mhconds55", "mhconds56", "mhconds57", "mhconds59", "mhconds60", "mhconds61", "mhconds62", "mhconds63",

                         "hcondns1", "hcondns3", "hcondns4", "hcondns5", "hcondns6", "hcondns7", "hcondns8",
                         "hcondns10", "hcondns11", "hcondns12", "hcondns15", "hcondns16", "hcondns19",
                         "hcondns21", "hcondns23", "hcondns24", "hcondns26", "hcondns27", "hcondns28", "hcondns29",
                         "hcondns30", "hcondns31", "hcondns33", "hcondns34", "hcondns35", "hcondns37", "hcondns38", "hcondns39",
                         "hcondns40", "hcondns41", "hcondns42",
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
                         "auditc1","auditc2","auditc3","auditc4","auditc5",
                         ## weight
                         "weight_lw","weight_xw"))

  data[, wave := "UKHLS Wave 13"]
  data[, wave_no := 13]
  data[, bhps_sample := ifelse(!is.na(pid),TRUE,FALSE)]
  data[, dataset := "UKHLS"]
  data[, id := ifelse(bhps_sample==FALSE, pidp, pid)]

  ########################################
  ######## ADD IN HOUSEHOLD DATA #########

  cat(crayon::green("\tHousehold..."))

  data.hhold <- data.table::fread(
    paste0(path, "/m_hhresp.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(data.hhold, names(data.hhold), tolower(names(data.hhold)))

  hhold_vars          <- Hmisc::Cs(m_hidp, m_tenure_dv, m_nkids_dv, m_hhsize, m_hhtype_dv,
                                   m_nch02_dv, m_nch34_dv, m_nch511_dv, m_nch1215_dv,
                                   m_fihhmngrs1_dv, m_fihhmnlabgrs_dv,
                                   m_fihhmnnet1_dv, m_fihhmnlabnet_dv, m_fihhmnsben_dv)

  data.hhold <- data.hhold[ , hhold_vars, with = F]
  data.table::setnames(data.hhold,
                       # old names
                       c("m_hidp", "m_tenure_dv", "m_nkids_dv", "m_hhsize","m_hhtype_dv",
                         "m_nch02_dv", "m_nch34_dv", "m_nch511_dv", "m_nch1215_dv",
                         "m_fihhmngrs1_dv", "m_fihhmnlabgrs_dv",
                         "m_fihhmnnet1_dv", "m_fihhmnlabnet_dv", "m_fihhmnsben_dv"),
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
    paste0(path, "/m_indall.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(data.indall, names(data.indall), tolower(names(data.indall)))

  indalm_vars  <- colnames(data.indall[ , c("pidp","m_hidp",
                                            "m_imd2019qe_dv","m_imd2017qni_dv",
                                            "m_imd2020qs_dv","m_imd2019qw_dv")])

  data.indall <- data.indall[ , indalm_vars, with = F]
  data.table::setnames(data.indall,
                       # old names
                       c("pidp","m_hidp",
                         "m_imd2019qe_dv","m_imd2017qni_dv",
                         "m_imd2020qs_dv","m_imd2019qw_dv"),
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
