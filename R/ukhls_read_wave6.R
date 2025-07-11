#' Read Understanding Society Wave 6
#'
#' Reads and performs basic cleaning operations on the UKHLS sixth wave. Missing values as detailed below are all set to NA.
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
ukhls_read_wave6 <- function(
  root = c("X:/"),
  file = "HAR_PR/PR/USoc/Data/SN6614_2024_12_02/tab/ukhls/",
  full = TRUE
) {

  cat(crayon::blue(crayon::underline("\tReading UKHLS Wave 6 datasets")))

  cat(crayon::green("\tIndividual..."))

  path <- here::here(paste0(root, file))

  data <- data.table::fread(
    paste0(path, "/f_indresp.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-10", "-90", "-90.0", "N/A")
  )
  if (full == TRUE) {
    # retain full interviews only
    data <- data[f_ivfio==1,]
  }

  data.table::setnames(data, names(data), tolower(names(data)))

  id_vars          <- Hmisc::Cs(pidp, pid, f_hidp,f_pno, f_psu, f_strata, f_istrtdaty, f_istrtdatm, f_istrtdatd)
  demographic_vars <- Hmisc::Cs(f_sex, f_dvage, f_birthy, f_gor_dv, f_urban_dv, f_mlstat,  f_marstat)
  prev_wave_vars   <- Hmisc::Cs(f_notempchk, f_empchk)
  econ_stat_vars   <- Hmisc::Cs(f_jbstat, f_jbhas, f_jboff, f_jboffy, f_jbterm1, f_jbterm2, f_jbsemp, f_jbpen, f_jbpenm)
  income_vars      <- Hmisc::Cs(f_fimnnet_dv, f_fimngrs_dv,
                                f_fimnlabnet_dv, f_fimnmisc_dv, f_fimnprben_dv, f_fimninvnet_dv, f_fimnpen_dv, f_fimnsben_dv)
  work_vars        <- Hmisc::Cs(f_paygu_dv, f_payg_dv, f_jbhrs, f_fimnlabgrs_dv, f_seearngrs_dv, f_jbsic07_cc, f_jbot, f_jbotpd,
                                f_jbnssec_dv, f_jbnssec3_dv, f_jbnssec5_dv, f_jbnssec8_dv, f_jbsize)
  employees_vars   <- Hmisc::Cs(f_paygl, f_paynl, f_payu, f_payug, f_ovtpay, f_extnsa, f_extrate, f_extrest, f_basnsa, f_basrate, f_basrest, f_ovtnsa, f_ovtrate, f_ovtrest)
  s.emp_vars       <- Hmisc::Cs(f_jshrs, f_jspayu, f_jspytx, f_jspyni)
  non.emp_vars     <- Hmisc::Cs(f_jbhad)
  job2_vars        <- Hmisc::Cs(f_j2has, f_j2semp, f_j2hrs, f_j2pay)
  benefits_vars    <- Hmisc::Cs(f_benbase1, f_benbase2, f_benbase3, f_benbase4, f_benbase96,
                                f_benctc)
  pension_vars     <- Hmisc::Cs(f_benpen1, f_benpen2, f_benpen3, f_benpen4, f_benpen5, f_benpen6, f_benpen7, f_benpen8, f_benpen96,
                                f_niserps)
  bendis_vars      <- Hmisc::Cs(f_bendis1, f_bendis2, f_bendis3, f_bendis4, f_bendis5, f_bendis12,
                                f_bendis7, f_bendis8, f_bendis10, f_bendis97, f_bendis96)
  otherben_vars    <- Hmisc::Cs(f_benesa,
                                f_othben1, f_othben2, f_othben3, f_othben4, f_othben5, f_othben6, f_othben7, f_othben8, f_othben9, f_othben97, f_othben96)
  benincome_vars   <- Hmisc::Cs(f_bensta2, f_bensta3, f_bensta4, f_bensta5, f_bensta6, f_bensta7, f_bensta97, f_bensta96)
  hhfinance_vars   <- Hmisc::Cs(f_fiyrdia, f_fiyrdb1, f_fiyrdb2, f_fiyrdb3, f_fiyrdb4, f_fiyrdb5, f_fiyrdb6, f_finnow, f_finfut)
  education_vars   <- Hmisc::Cs(f_hiqual_dv)
  health_vars      <- Hmisc::Cs(f_health, f_aidhh, f_sclfsat1, f_sclfsato, f_sf12pcs_dv, f_sf12mcs_dv,
                                f_scsf1, f_scsf2a, f_scsf2b, f_scsf3a, f_scsf3b, f_scsf4a, f_scsf4b, f_scsf5, f_scsf6a, f_scsf6b, f_scsf6c, f_scsf7,
                                f_scghq1_dv,f_scghq2_dv)
  preg_vars        <- Hmisc::Cs(f_preg,
                                f_pregout1, f_pregend1, f_pregsmoke1, f_smkmnth11, f_smkmnth21, f_smkmnth31, f_pregsmk11, f_pregsmk21, f_pregsmk31, f_aedrof1, f_aepuwk1, f_aepuda1, f_lchmulti1,
                                f_pregout2, f_pregend2, f_pregsmoke2, f_smkmnth12, f_smkmnth22, f_smkmnth32, f_pregsmk12, f_pregsmk22, f_pregsmk32, f_aedrof2, f_aepuwk2, f_aepuda2, f_lchmulti2,
                                f_pregout3, f_pregend3, f_pregsmoke3, f_smkmnth13, f_smkmnth23, f_smkmnth33, f_pregsmk13, f_pregsmk23, f_pregsmk33, f_aedrof3, f_aepuwk3, f_aepuda3, f_lchmulti3,
                                f_pregout4, f_pregend4, f_pregsmoke4, f_smkmnth14, f_smkmnth24, f_smkmnth34, f_pregsmk14, f_pregsmk24, f_pregsmk34, f_aedrof4, f_aepuwk4, f_aepuda4, f_lchmulti4,
                                f_pregout5, f_pregend5, f_pregsmoke5, f_smkmnth15, f_smkmnth25, f_smkmnth35, f_pregsmk15, f_pregsmk25, f_pregsmk35, f_aedrof5, f_aepuwk5, f_aepuda5, f_lchmulti5,
                                f_nnewborn)
  smoke_vars       <- Hmisc::Cs(f_smoker, f_ncigs)
  alc_vars         <- Hmisc::Cs(f_dklm, f_drnk4w,f_evralc, f_fivealcdr)
  weight_vars      <- Hmisc::Cs(f_indinus_lw, f_indinui_xw)


  names <- c(id_vars, demographic_vars, prev_wave_vars, econ_stat_vars, income_vars, work_vars, employees_vars, s.emp_vars, non.emp_vars, job2_vars,
             benefits_vars, pension_vars, bendis_vars, otherben_vars, benincome_vars, hhfinance_vars, education_vars, health_vars, preg_vars, smoke_vars, alc_vars, weight_vars)
  names <- tolower(names)

  data <- data[ , names, with = F]

  data.table::setnames(data,

                       c("pidp","pid","f_hidp","f_pno","f_psu","f_strata","f_istrtdaty","f_istrtdatm","f_istrtdatd",
                         ## demographic
                         "f_sex","f_dvage","f_birthy","f_gor_dv","f_urban_dv","f_mlstat","f_marstat",
                         ## previous wave variables
                         "f_notempchk","f_empchk",
                         ## economic status
                         "f_jbstat","f_jbhas","f_jboff","f_jboffy","f_jbterm1","f_jbterm2","f_jbsemp","f_jbpen","f_jbpenm",
                         ## income variables
                         "f_fimnnet_dv", "f_fimngrs_dv",
                         "f_fimnlabnet_dv", "f_fimnmisc_dv", "f_fimnprben_dv", "f_fimninvnet_dv", "f_fimnpen_dv", "f_fimnsben_dv",
                         ## work variables
                         "f_paygu_dv","f_payg_dv","f_jbhrs","f_fimnlabgrs_dv","f_seearngrs_dv","f_jbsic07_cc","f_jbot","f_jbotpd",
                         "f_jbnssec_dv","f_jbnssec3_dv","f_jbnssec5_dv","f_jbnssec8_dv", "f_jbsize",
                         ## employees
                         "f_paygl","f_paynl","f_payu","f_payug","f_ovtpay","f_extnsa","f_extrate","f_extrest","f_basnsa","f_basrate",
                         "f_basrest","f_ovtnsa","f_ovtrate","f_ovtrest",
                         ## self-employed
                         "f_jshrs","f_jspayu","f_jspytx","f_jspyni",
                         ## non-employed
                         "f_jbhad",
                         ## second job
                         "f_j2has","f_j2semp","f_j2hrs","f_j2pay",
                         ## benefits
                         "f_benbase1","f_benbase2","f_benbase3","f_benbase4","f_benbase96",
                         "f_benctc",
                         ## pensions
                         "f_benpen1","f_benpen2","f_benpen3","f_benpen4","f_benpen5","f_benpen6","f_benpen7","f_benpen8","f_benpen96",
                         "f_niserps",
                         ## disability benefits
                         "f_bendis1","f_bendis2","f_bendis3","f_bendis4","f_bendis5","f_bendis12",
                         "f_bendis7","f_bendis8","f_bendis10","f_bendis97","f_bendis96",
                         ## other benefits
                         "f_benesa","f_othben1","f_othben2","f_othben3","f_othben4","f_othben5","f_othben6",
                         "f_othben7","f_othben8","f_othben9","f_othben97","f_othben96",
                         ## benefit income variables
                         "f_bensta2","f_bensta3","f_bensta4","f_bensta5","f_bensta6","f_bensta7","f_bensta97","f_bensta96",
                         ## household finance variables (interest and dividends)
                         "f_fiyrdia","f_fiyrdb1","f_fiyrdb2","f_fiyrdb3","f_fiyrdb4","f_fiyrdb5","f_fiyrdb6","f_finnow","f_finfut",
                         ## education variables
                         "f_hiqual_dv",
                         ## health variables
                         "f_health","f_aidhh","f_sclfsat1","f_sclfsato","f_sf12pcs_dv","f_sf12mcs_dv",
                         "f_scsf1","f_scsf2a","f_scsf2b","f_scsf3a","f_scsf3b","f_scsf4a","f_scsf4b","f_scsf5","f_scsf6a","f_scsf6b","f_scsf6c","f_scsf7",
                         "f_scghq1_dv","f_scghq2_dv",
                         ## pregnancy variables
                         "f_preg","f_pregout1","f_pregend1","f_pregsmoke1","f_smkmnth11","f_smkmnth21","f_smkmnth31","f_pregsmk11","f_pregsmk21","f_pregsmk31","f_aedrof1","f_aepuwk1","f_aepuda1","f_lchmulti1",
                         "f_pregout2","f_pregend2","f_pregsmoke2","f_smkmnth12","f_smkmnth22","f_smkmnth32","f_pregsmk12","f_pregsmk22","f_pregsmk32","f_aedrof2","f_aepuwk2","f_aepuda2","f_lchmulti2",
                         "f_pregout3","f_pregend3","f_pregsmoke3","f_smkmnth13","f_smkmnth23","f_smkmnth33","f_pregsmk13","f_pregsmk23","f_pregsmk33","f_aedrof3","f_aepuwk3","f_aepuda3","f_lchmulti3",
                         "f_pregout4","f_pregend4","f_pregsmoke4","f_smkmnth14","f_smkmnth24","f_smkmnth34","f_pregsmk14","f_pregsmk24","f_pregsmk34","f_aedrof4","f_aepuwk4","f_aepuda4","f_lchmulti4",
                         "f_pregout5","f_pregend5","f_pregsmoke5","f_smkmnth15","f_smkmnth25","f_smkmnth35","f_pregsmk15","f_pregsmk25","f_pregsmk35","f_aedrof5","f_aepuwk5","f_aepuda5","f_lchmulti5",
                         "f_nnewborn",
                         ## smoking variables
                         "f_smoker", "f_ncigs",
                         ## alcohol variables
                         "f_dklm","f_drnk4w","f_evralc","f_fivealcdr",
                         ## weight
                         "f_indinus_lw","f_indinui_xw"),

                       c("pidp","pid","hidp","person_number","psu","strata","year","month","day",
                         ## demographic
                         "sex","age","birth_year","region","urban","mlstat","marstat",
                         ## previous wave variables
                         "notempchk","empchk",
                         ## economic status
                         "econ_stat","jbhas","jboff","jboffy","jbterm1","jbterm2","jbsemp","jbpen","jbpen_member",
                         ## income variables
                         "fimnnet_dv", "fimngrs_dv",
                         "fimnlabnet_dv", "fimnmisc_dv", "fimnprben_dv", "fimninvnet_dv", "fimnpen_dv", "fimnsben_dv",
                         ## work variables
                         "grss_pay_usual","grss_pay_last","hours","grss_lab_inc","grss_semp","sic07","ovthours_pw","ovthours_paid",
                         "nssec","nssec_3cat","nssec_5cat","nssec_8cat", "jbsize",
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
                         ## benefit income variables
                         "bensta_edugrant","bensta_tupay","bensta_alimony","bensta_fampay","bensta_rentlodge","bensta_rentother","bensta_other","non_bensta",
                         ## household finance variables
                         "fiyrdia","fiyrdb1","fiyrdb2","fiyrdb3","fiyrdb4","fiyrdb5","fiyrdb6","finnow","finfut",
                         ## education variables
                         "highest_qual",
                         ## health variables
                         "lt_sick","caring","health_satisf","life_satisf","sf12_pcs","sf12_mcs",
                         "sf1","sf2a","sf2b","sf3a","sf3b","sf4a","sf4b","sf5","sf6a","sf6b","sf6c","sf7",
                         "ghq1","ghq2",
                         ## pregnancy variables
                         "preg",
                         "pregout1","pregend1","pregsmoke1","smkmnth11","smkmnth21","smkmnth31","pregsmk_ncigs11","pregsmk_ncigs21","pregsmk_ncigs31","pregdrnk_freq1","pregdrnk_unitpw1","pregdrnk_unit1","lchmulti1",
                         "pregout2","pregend2","pregsmoke2","smkmnth12","smkmnth22","smkmnth32","pregsmk_ncigs12","pregsmk_ncigs22","pregsmk_ncigs32","pregdrnk_freq2","pregdrnk_unitpw2","pregdrnk_unit2","lchmulti2",
                         "pregout3","pregend3","pregsmoke3","smkmnth13","smkmnth23","smkmnth33","pregsmk_ncigs13","pregsmk_ncigs23","pregsmk_ncigs33","pregdrnk_freq3","pregdrnk_unitpw3","pregdrnk_unit3","lchmulti3",
                         "pregout4","pregend4","pregsmoke4","smkmnth14","smkmnth24","smkmnth34","pregsmk_ncigs14","pregsmk_ncigs24","pregsmk_ncigs34","pregdrnk_freq4","pregdrnk_unitpw4","pregdrnk_unit4","lchmulti4",
                         "pregout5","pregend5","pregsmoke5","smkmnth15","smkmnth25","smkmnth35","pregsmk_ncigs15","pregsmk_ncigs25","pregsmk_ncigs35","pregdrnk_freq5","pregdrnk_unitpw5","pregdrnk_unit5","lchmulti5",
                         "nnewborn",
                         ## smoking variables
                         "smoker", "ncigs",
                         ## alcohol variables
                         "dklm","drnk4w","evralc","fivealcdr",
                         ## weight
                         "weight_lw","weight_xw"))

  data[, wave := "UKHLS Wave 6"]
  data[, wave_no := 6]
  data[, bhps_sample := ifelse(!is.na(pid),TRUE,FALSE)]
  data[, dataset := "UKHLS"]
  data[, id := ifelse(bhps_sample==FALSE, pidp, pid)]

  ########################################
  ######## ADD IN HOUSEHOLD DATA #########

  cat(crayon::green("\tHousehold..."))

  data.hhold <- data.table::fread(
    paste0(path, "/f_hhresp.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(data.hhold, names(data.hhold), tolower(names(data.hhold)))

  hhold_vars          <- Hmisc::Cs(f_hidp, f_tenure_dv, f_nkids_dv, f_hhsize, f_hhtype_dv,
                                   f_nch02_dv, f_nch34_dv, f_nch511_dv, f_nch1215_dv,
                                   f_fihhmngrs1_dv, f_fihhmnlabgrs_dv,
                                   f_fihhmnnet1_dv, f_fihhmnlabnet_dv, f_fihhmnsben_dv)

  data.hhold <- data.hhold[ , hhold_vars, with = F]
  data.table::setnames(data.hhold,
                       # old names
                       c("f_hidp","f_tenure_dv","f_nkids_dv","f_hhsize","f_hhtype_dv",
                         "f_nch02_dv","f_nch34_dv","f_nch511_dv","f_nch1215_dv",
                         "f_fihhmngrs1_dv", "f_fihhmnlabgrs_dv",
                         "f_fihhmnnet1_dv", "f_fihhmnlabnet_dv", "f_fihhmnsben_dv"),
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
    paste0(path, "/f_indall.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(data.indall, names(data.indall), tolower(names(data.indall)))

  indall_vars  <- colnames(data.indall[ , c("pidp","f_hidp",
                                            "f_imd2019qe_dv","f_imd2017qni_dv",
                                            "f_imd2020qs_dv","f_imd2019qw_dv")])

  data.indall <- data.indall[ , indall_vars, with = F]
  data.table::setnames(data.indall,
                       # old names
                       c("pidp","f_hidp",
                         "f_imd2019qe_dv","f_imd2017qni_dv",
                         "f_imd2020qs_dv","f_imd2019qw_dv"),
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
