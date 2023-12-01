#' Read Understanding Society Wave 8
#'
#' Reads and performs basic cleaning operations on the UKHLS eighth wave. Missing values as detailed below are all set to NA.
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
ukhls_read_wave8 <- function(
  root = c("X:/"),
  file = "HAR_PR/PR/USoc/Data/SN6614_2023_11_24/tab/ukhls/",
  full = TRUE
) {

  cat(crayon::blue(crayon::underline("\tReading UKHLS Wave 8 datasets")))

  cat(crayon::green("\tIndividual..."))

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
  work_vars        <- Hmisc::Cs(h_paygu_dv, h_payg_dv, h_jbhrs, h_fimnlabgrs_dv, h_seearngrs_dv, h_jbsic07_cc, h_jbot, h_jbotpd,
                                h_jbnssec_dv, h_jbnssec3_dv, h_jbnssec5_dv, h_jbnssec8_dv)
  employees_vars   <- Hmisc::Cs(h_paygl, h_paynl, h_payu, h_payug, h_paytyp, h_ovtpay, h_pvtpyset, h_extrate, h_extrest, h_basnset, h_basrate, h_basrest, h_ovtnset, h_ovtrate, h_ovtrest)
  s.emp_vars       <- Hmisc::Cs(h_jshrs, h_jspayu, h_jspytx, h_jspyni)
  non.emp_vars     <- Hmisc::Cs(h_jbhad)
  job2_vars        <- Hmisc::Cs(h_j2has, h_j2semp, h_j2hrs, h_j2pay)
  benefits_vars    <- Hmisc::Cs(h_benbase1, h_benbase2, h_benbase3, h_benbase4, h_benbase96,
                                h_benctc)
  pension_vars     <- Hmisc::Cs(h_benpen1, h_benpen2, h_benpen3, h_benpen4, h_benpen5, h_benpen6, h_benpen7, h_benpen8, h_benpen96,
                                h_niserps)
  bendis_vars      <- Hmisc::Cs(h_bendis1, h_bendis2, h_bendis3, h_bendis4, h_bendis5, h_bendis12,
                                h_bendis7, h_bendis8, h_bendis10, h_bendis97, h_bendis96)
  otherben_vars    <- Hmisc::Cs(h_benesa,
                                h_othben1, h_othben2, h_othben3, h_othben4, h_othben5, h_othben6, h_othben7, h_othben8, h_othben9, h_othben97, h_othben96)
  benincome_vars   <- Hmisc::Cs(h_bensta2, h_bensta3, h_bensta4, h_bensta5, h_bensta6, h_bensta7, h_bensta97, h_bensta96)
  hhfinance_vars   <- Hmisc::Cs(h_fiyrdia, h_fiyrdb1, h_fiyrdb2, h_fiyrdb3, h_fiyrdb4, h_fiyrdb5, h_fiyrdb6, h_finnow, h_finfut)
  education_vars   <- Hmisc::Cs(h_hiqual_dv)
  health_vars      <- Hmisc::Cs(h_health, h_aidhh, h_sclfsat1, h_sclfsato, h_sf12pcs_dv, h_sf12mcs_dv,
                                h_scsf1, h_scsf2a, h_scsf2b, h_scsf3a, h_scsf3b, h_scsf4a, h_scsf4b, h_scsf5, h_scsf6a, h_scsf6b,h_scsf6c, h_scsf7)
  preg_vars        <- Hmisc::Cs(h_preg,
                                h_pregout1, h_pregend1, h_pregsmoke1, h_smkmnth11, h_smkmnth21, h_smkmnth31, h_pregsmk11, h_pregsmk21, h_pregsmk31, h_aedrof1, h_aepuwk1, h_aepuda1, h_lchmulti1,
                                h_pregout2, h_pregend2, h_pregsmoke2, h_smkmnth12, h_smkmnth22, h_smkmnth32, h_pregsmk12, h_pregsmk22, h_pregsmk32, h_aedrof2, h_aepuwk2, h_aepuda2, h_lchmulti2,
                                h_pregout3, h_pregend3, h_pregsmoke3, h_smkmnth13, h_smkmnth23, h_smkmnth33, h_pregsmk13, h_pregsmk23, h_pregsmk33, h_aedrof3, h_aepuwk3, h_aepuda3, h_lchmulti3,
                                h_pregout4, h_pregend4, h_pregsmoke4, h_smkmnth14, h_smkmnth24, h_smkmnth34, h_pregsmk14, h_pregsmk24, h_pregsmk34, h_aedrof4, h_aepuwk4, h_aepuda4, h_lchmulti4,
                                h_nnewborn)
  smoke_vars       <- Hmisc::Cs(h_smoker, h_ncigs, h_ecigs)
  alc_vars         <- Hmisc::Cs(h_dklm, h_drnk4w, h_evralc, h_fivealcdr)
  weight_vars      <- Hmisc::Cs(h_indinus_lw, h_indinui_xw)


  names <- c(id_vars, demographic_vars, prev_wave_vars, econ_stat_vars, work_vars, employees_vars, s.emp_vars, non.emp_vars, job2_vars, benefits_vars, pension_vars, bendis_vars, otherben_vars, benincome_vars, hhfinance_vars, education_vars, health_vars, preg_vars, smoke_vars, alc_vars, weight_vars)
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
                         "h_jbnssec_dv","h_jbnssec3_dv","h_jbnssec5_dv","h_jbnssec8_dv",
                         ## employees
                         "h_paygl","h_paynl","h_payu","h_payug","h_paytyp","h_ovtpay","h_pvtpyset","h_extrate","h_extrest","h_basnset","h_basrate",
                         "h_basrest","h_ovtnset","h_ovtrate","h_ovtrest",
                         ## self-employed
                         "h_jshrs","h_jspayu","h_jspytx","h_jspyni",
                         ## non-employed
                         "h_jbhad",
                         ## second job
                         "h_j2has","h_j2semp","h_j2hrs","h_j2pay",
                         ## benefits
                         "h_benbase1","h_benbase2","h_benbase3","h_benbase4","h_benbase96",
                         "h_benctc",
                         ## pensions
                         "h_benpen1","h_benpen2","h_benpen3","h_benpen4","h_benpen5","h_benpen6","h_benpen7","h_benpen8","h_benpen96",
                         "h_niserps",
                         ## disability benefits
                         "h_bendis1","h_bendis2","h_bendis3","h_bendis4","h_bendis5","h_bendis12",
                         "h_bendis7","h_bendis8","h_bendis10","h_bendis97","h_bendis96",
                         ## other benefits
                         "h_benesa",
                         "h_othben1","h_othben2","h_othben3","h_othben4","h_othben5","h_othben6","h_othben7","h_othben8","h_othben9","h_othben97","h_othben96",
                         ## benefit income variables (formerly receivables)
                         "h_bensta2","h_bensta3","h_bensta4","h_bensta5","h_bensta6","h_bensta7","h_bensta97","h_bensta96",
                         ## household finance variables (interest and dividends)
                         "h_fiyrdia","h_fiyrdb1","h_fiyrdb2","h_fiyrdb3","h_fiyrdb4","h_fiyrdb5","h_fiyrdb6","h_finnow","h_finfut",
                         ## education variables
                         "h_hiqual_dv",
                         ## health variables
                         "h_health","h_aidhh","h_sclfsat1","h_sclfsato","h_sf12pcs_dv","h_sf12mcs_dv",
                         "h_scsf1","h_scsf2a","h_scsf2b","h_scsf3a","h_scsf3b","h_scsf4a","h_scsf4b","h_scsf5","h_scsf6a","h_scsf6b","h_scsf6c","h_scsf7",
                         ## pregnancy variables
                         "h_preg",
                         "h_pregout1","h_pregend1","h_pregsmoke1","h_smkmnth11","h_smkmnth21","h_smkmnth31","h_pregsmk11","h_pregsmk21","h_pregsmk31","h_aedrof1","h_aepuwk1","h_aepuda1","h_lchmulti1",
                         "h_pregout2","h_pregend2","h_pregsmoke2","h_smkmnth12","h_smkmnth22","h_smkmnth32","h_pregsmk12","h_pregsmk22","h_pregsmk32","h_aedrof2","h_aepuwk2","h_aepuda2","h_lchmulti2",
                         "h_pregout3","h_pregend3","h_pregsmoke3","h_smkmnth13","h_smkmnth23","h_smkmnth33","h_pregsmk13","h_pregsmk23","h_pregsmk33","h_aedrof3","h_aepuwk3","h_aepuda3","h_lchmulti3",
                         "h_pregout4","h_pregend4","h_pregsmoke4","h_smkmnth14","h_smkmnth24","h_smkmnth34","h_pregsmk14","h_pregsmk24","h_pregsmk34","h_aedrof4","h_aepuwk4","h_aepuda4","h_lchmulti4",
                         "h_nnewborn",
                         ## smoking variables
                         "h_smoker", "h_ncigs","h_ecigs",
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
                         "nssec","nssec_3cat","nssec_5cat","nssec_8cat",
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
                         "income_serps",
                         ## disability benefits
                         "bendis1","bendis2","bendis3","bendis4","bendis5","bendis12",
                         "bendis7","bendis8","bendis10","bendis97","bendis96",
                         ## other benefits
                         "benesa","othben1","othben2","othben3","othben4","othben5","othben6",
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
                         "pregout4","pregend4","pregsmoke4","smkmnth14","smkmnth24","smkmnth34","pregsmk_ncigs14","pregsmk_ncigs24","pregsmk_ncigs34","pregdrnk_freq4","pregdrnk_unitpw4","pregdrnk_unit4","lchmulti4",
                         "nnewborn",
                         ## smoking variables
                         "smoker", "ncigs","ecigs",
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

  cat(crayon::green("\tHousehold..."))

  data.hhold <- data.table::fread(
    paste0(path, "/h_hhresp.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(data.hhold, names(data.hhold), tolower(names(data.hhold)))

  hhold_vars          <- Hmisc::Cs(h_hidp, h_tenure_dv, h_nkids_dv, h_hhsize, h_hhtype_dv,
                                   h_nch02_dv, h_nch34_dv, h_nch511_dv, h_nch1215_dv,
                                   h_fihhmngrs1_dv, h_fihhmnlabgrs_dv,
                                   h_fihhmnnet1_dv, h_fihhmnlabnet_dv, h_fihhmnsben_dv)

  data.hhold <- data.hhold[ , hhold_vars, with = F]
  data.table::setnames(data.hhold,
                       # old names
                       c("h_hidp","h_tenure_dv","h_nkids_dv","h_hhsize","h_hhtype_dv",
                         "h_nch02_dv","h_nch34_dv","h_nch511_dv","h_nch1215_dv",
                         "h_fihhmngrs1_dv", "h_fihhmnlabgrs_dv",
                         "h_fihhmnnet1_dv", "h_fihhmnlabnet_dv", "h_fihhmnsben_dv"),
                       # new names
                       c("hidp","hh_tenure","hh_numchild","hh_size","hh_type",
                         "hh_numchild02","hh_numchild34","hh_numchild511","hh_numchild1215",
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
    paste0(path, "/h_indall.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(data.indall, names(data.indall), tolower(names(data.indall)))

  indall_vars  <- colnames(data.indall[ , c("pidp","h_hidp",
                                            "h_imd2019qe_dv","h_imd2017qni_dv",
                                            "h_imd2020qs_dv","h_imd2019qw_dv")])

  data.indall <- data.indall[ , indall_vars, with = F]
  data.table::setnames(data.indall,
                       # old names
                       c("pidp","h_hidp",
                         "h_imd2019qe_dv","h_imd2017qni_dv",
                         "h_imd2020qs_dv","h_imd2019qw_dv"),
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

  return(data_merged[])
}
